environment            = "dev"
location               = "eastus"
name_prefix            = "oktodo"
owner                  = "portfolio"
aks_node_count         = 2
aks_node_vm_size       = "Standard_B2s"
log_retention_days     = 30
vnet_address_space     = ["10.42.0.0/16"]
aks_subnet_prefixes    = ["10.42.1.0/24"]
admin_group_object_ids = []

tags = {
  "cost-center" = "portfolio"
}
