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
