data "azurerm_client_config" "current" {}

locals {
  resource_prefix = "${var.name_prefix}-${var.environment}"
  common_tags = merge(
    {
      application  = "overkilled-todo"
      environment  = var.environment
      "managed-by" = "terraform"
      owner        = var.owner
      milestone    = "aks-gitops"
    },
    var.tags
  )
}

resource "azurerm_resource_group" "platform" {
  name     = "rg-${local.resource_prefix}"
  location = var.location
  tags     = local.common_tags
}

resource "azurerm_virtual_network" "platform" {
  name                = "vnet-${local.resource_prefix}"
  location            = azurerm_resource_group.platform.location
  resource_group_name = azurerm_resource_group.platform.name
  address_space       = var.vnet_address_space
  tags                = local.common_tags
}

resource "azurerm_subnet" "aks" {
  name                 = "snet-aks"
  resource_group_name  = azurerm_resource_group.platform.name
  virtual_network_name = azurerm_virtual_network.platform.name
  address_prefixes     = var.aks_subnet_prefixes
}

resource "azurerm_container_registry" "platform" {
  name                = replace("${var.name_prefix}${var.environment}acr", "-", "")
  resource_group_name = azurerm_resource_group.platform.name
  location            = azurerm_resource_group.platform.location
  sku                 = "Basic"
  admin_enabled       = false
  tags                = local.common_tags
}

resource "azurerm_log_analytics_workspace" "platform" {
  name                = "log-${local.resource_prefix}"
  location            = azurerm_resource_group.platform.location
  resource_group_name = azurerm_resource_group.platform.name
  sku                 = "PerGB2018"
  retention_in_days   = var.log_retention_days
  tags                = local.common_tags
}

resource "azurerm_key_vault" "platform" {
  name                       = substr(replace("kv-${local.resource_prefix}", "-", ""), 0, 24)
  location                   = azurerm_resource_group.platform.location
  resource_group_name        = azurerm_resource_group.platform.name
  tenant_id                  = data.azurerm_client_config.current.tenant_id
  sku_name                   = "standard"
  soft_delete_retention_days = 7
  purge_protection_enabled   = true
  enable_rbac_authorization  = true
  tags                       = local.common_tags
}

resource "azurerm_user_assigned_identity" "external_secrets" {
  name                = "id-${local.resource_prefix}-external-secrets"
  location            = azurerm_resource_group.platform.location
  resource_group_name = azurerm_resource_group.platform.name
  tags                = local.common_tags
}

resource "azurerm_user_assigned_identity" "flux" {
  name                = "id-${local.resource_prefix}-flux"
  location            = azurerm_resource_group.platform.location
  resource_group_name = azurerm_resource_group.platform.name
  tags                = local.common_tags
}

resource "azurerm_kubernetes_cluster" "platform" {
  name                = "aks-${local.resource_prefix}"
  location            = azurerm_resource_group.platform.location
  resource_group_name = azurerm_resource_group.platform.name
  dns_prefix          = "${local.resource_prefix}-aks"
  kubernetes_version  = var.aks_kubernetes_version
  tags                = local.common_tags

  role_based_access_control_enabled = true

  default_node_pool {
    name           = "system"
    node_count     = var.aks_node_count
    vm_size        = var.aks_node_vm_size
    vnet_subnet_id = azurerm_subnet.aks.id
  }

  identity {
    type = "SystemAssigned"
  }

  azure_active_directory_role_based_access_control {
    managed                = true
    azure_rbac_enabled     = true
    admin_group_object_ids = var.admin_group_object_ids
  }

  oms_agent {
    log_analytics_workspace_id = azurerm_log_analytics_workspace.platform.id
  }

  workload_identity_enabled = true
  oidc_issuer_enabled       = true
}

resource "azurerm_role_assignment" "aks_pull_acr" {
  scope                = azurerm_container_registry.platform.id
  role_definition_name = "AcrPull"
  principal_id         = azurerm_kubernetes_cluster.platform.kubelet_identity[0].object_id
}

resource "azurerm_role_assignment" "external_secrets_key_vault_user" {
  scope                = azurerm_key_vault.platform.id
  role_definition_name = "Key Vault Secrets User"
  principal_id         = azurerm_user_assigned_identity.external_secrets.principal_id
}
