variable "environment" {
  description = "Environment name used for Azure resource naming and tags."
  type        = string

  validation {
    condition     = can(regex("^[a-z0-9-]{2,12}$", var.environment))
    error_message = "Environment must be 2-12 characters of lowercase letters, numbers, or hyphens."
  }
}

variable "location" {
  description = "Azure region for all resources."
  type        = string
}

variable "name_prefix" {
  description = "Short prefix used in Azure resource names."
  type        = string
  default     = "oktodo"

  validation {
    condition     = can(regex("^[a-z0-9]{2,10}$", var.name_prefix))
    error_message = "Name prefix must be 2-10 lowercase alphanumeric characters."
  }
}

variable "owner" {
  description = "Owner tag value for portfolio and cleanup tracking."
  type        = string
}

variable "aks_kubernetes_version" {
  description = "Optional AKS Kubernetes version. Leave null to use the default supported version for the region."
  type        = string
  default     = null
}

variable "aks_node_count" {
  description = "Initial node count for the AKS system node pool."
  type        = number
  default     = 2
}

variable "aks_node_vm_size" {
  description = "VM size for the AKS system node pool."
  type        = string
  default     = "Standard_B2s"
}

variable "vnet_address_space" {
  description = "Address space for the AKS virtual network."
  type        = list(string)
  default     = ["10.42.0.0/16"]
}

variable "aks_subnet_prefixes" {
  description = "Subnet CIDR blocks used by AKS nodes."
  type        = list(string)
  default     = ["10.42.1.0/24"]
}

variable "log_retention_days" {
  description = "Retention period for Log Analytics workspace data."
  type        = number
  default     = 30
}

variable "admin_group_object_ids" {
  description = "Azure AD group object IDs granted AKS cluster admin access."
  type        = list(string)
  default     = []
}

variable "tags" {
  description = "Additional tags applied to Azure resources."
  type        = map(string)
  default     = {}
}

variable "github_repository" {
  description = "Optional GitHub repository slug (owner/name) allowed to use OIDC for the image release workflow. Leave empty to skip creating the federated credential."
  type        = string
  default     = ""

  validation {
    condition     = var.github_repository == "" || can(regex("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", var.github_repository))
    error_message = "GitHub repository must be empty or use the owner/name format."
  }
}

variable "github_actions_ref" {
  description = "Git ref allowed to assume the GitHub Actions release identity through OIDC."
  type        = string
  default     = "refs/heads/master"
}
