# AKS Prod Overlay

This overlay is a production-shaped example for review and validation, not an immediate deployment target.

It demonstrates:

- Non-local ACR image references.
- Production hostname separation from local and dev.
- Higher frontend, gateway, and read-service replica counts.
- HPA policy for stateless request handlers.
- Larger MariaDB resources while documenting that managed Azure Database is the real production path.

Before production use, complete the Milestone 4 secret-management work and replace in-cluster MariaDB with Azure Database for MySQL Flexible Server.
