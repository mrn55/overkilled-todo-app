output "resource_group_name" {
  description = "Azure resource group containing the platform resources."
  value       = azurerm_resource_group.platform.name
}

output "aks_cluster_name" {
  description = "AKS cluster name."
  value       = azurerm_kubernetes_cluster.platform.name
}

output "acr_login_server" {
  description = "Azure Container Registry login server for image publishing."
  value       = azurerm_container_registry.platform.login_server
}

output "key_vault_name" {
  description = "Key Vault name reserved for External Secrets integration."
  value       = azurerm_key_vault.platform.name
}

output "log_analytics_workspace_id" {
  description = "Log Analytics workspace resource ID."
  value       = azurerm_log_analytics_workspace.platform.id
}

output "external_secrets_identity_client_id" {
  description = "User-assigned managed identity client ID for External Secrets."
  value       = azurerm_user_assigned_identity.external_secrets.client_id
}

output "flux_identity_client_id" {
  description = "User-assigned managed identity client ID reserved for Flux integrations."
  value       = azurerm_user_assigned_identity.flux.client_id
}

output "github_actions_release_identity_client_id" {
  description = "Client ID for the user-assigned managed identity used by the ACR image release workflow."
  value       = azurerm_user_assigned_identity.github_actions_release.client_id
}

output "github_actions_variable_commands" {
  description = "GitHub CLI commands that configure repository variables required by the ACR image release workflow."
  value       = <<-EOT
    gh variable set AZURE_CLIENT_ID --body '${azurerm_user_assigned_identity.github_actions_release.client_id}'
    gh variable set AZURE_TENANT_ID --body '${data.azurerm_client_config.current.tenant_id}'
    gh variable set AZURE_SUBSCRIPTION_ID --body '${data.azurerm_client_config.current.subscription_id}'
    gh variable set ACR_LOGIN_SERVER --body '${azurerm_container_registry.platform.login_server}'
  EOT
}
