[CmdletBinding()]
param(
    [string]$GatewayUrl = "http://127.0.0.1",
    [string]$FrontendUrl = "http://127.0.0.1:3000"
)

$ErrorActionPreference = "Stop"
$createdTodo = $null

try {
    $gatewayHealth = Invoke-RestMethod -Uri "$GatewayUrl/healthz"
    if ($gatewayHealth.Trim() -ne "ok") {
        throw "Gateway health check returned an unexpected response."
    }

    $frontendResponse = Invoke-WebRequest -UseBasicParsing -Uri $FrontendUrl
    if ($frontendResponse.StatusCode -ne 200) {
        throw "Frontend returned HTTP $($frontendResponse.StatusCode)."
    }

    $runtimeConfig = Invoke-WebRequest -UseBasicParsing -Uri "$FrontendUrl/config.js"
    if ($runtimeConfig.Content -notmatch [regex]::Escape("http://localhost/todo")) {
        throw "Frontend runtime configuration does not target the Docker Compose gateway."
    }

    $createdTodo = Invoke-RestMethod `
        -Method Post `
        -Uri "$GatewayUrl/todo" `
        -ContentType "application/json" `
        -Body (@{ title = "Local demo smoke test" } | ConvertTo-Json)

    $todos = Invoke-RestMethod -Uri "$GatewayUrl/todo"
    if (-not ($todos | Where-Object { $_.id -eq $createdTodo.id })) {
        throw "Created TODO $($createdTodo.id) was not returned by the read service."
    }

    $updatedTodo = Invoke-RestMethod `
        -Method Put `
        -Uri "$GatewayUrl/todo/$($createdTodo.id)" `
        -ContentType "application/json" `
        -Body (@{ title = "Local demo verified" } | ConvertTo-Json)

    if ($updatedTodo.title -ne "Local demo verified") {
        throw "Update service returned an unexpected title."
    }

    Invoke-RestMethod -Method Delete -Uri "$GatewayUrl/todo/$($createdTodo.id)" | Out-Null
    $deletedId = $createdTodo.id
    $createdTodo = $null

    $todosAfterDelete = Invoke-RestMethod -Uri "$GatewayUrl/todo"
    if ($todosAfterDelete | Where-Object { $_.id -eq $deletedId }) {
        throw "Deleted TODO $deletedId is still returned by the read service."
    }

    Write-Host "PASS: frontend, gateway, and create/read/update/delete services are demo-ready."
}
finally {
    if ($null -ne $createdTodo) {
        try {
            Invoke-RestMethod -Method Delete -Uri "$GatewayUrl/todo/$($createdTodo.id)" | Out-Null
        }
        catch {
            Write-Warning "Could not clean up smoke-test TODO $($createdTodo.id): $($_.Exception.Message)"
        }
    }
}
