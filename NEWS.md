# GalaxyR 0.1.1

-   Add support to download full histories i.e. as RO-CRATE by adding the `galaxy_download_rocrate()` function.

# GalaxyR 0.1.0 (Initial CRAN release)

-   Initial release of the Galaxy R client, providing a high-level and S4-based interface to Galaxy’s REST API. S4 Galaxy session object to carry state (history, datasets, invocation IDs, outputs, status) through pipe-based workflows.
-   Supports configuring Galaxy connection parameters via environment variables (`GALAXY_API_KEY`, `GALAXY_URL`) and helper `galaxy_initialize()` constructor.
-   History management: list histories, create new histories, inspect history sizes (robust to different Galaxy size fields), and include deleted items optionally.
-   Dataset operations: upload via HTTPS (and FTP helper), wait for completion, retrieve dataset metadata, download results with safe/unique file naming, and delete single or multiple datasets with optional purge.
-   Workflow operations: list workflows (including published), fetch workflow metadata and inputs, start workflow invocations, and poll workflow execution with job-level state tracking and result collection.
-   Tooling: list tools (with optional panel filtering), fetch detailed tool definitions, validate tool inputs, and run single tools with automatic dataset input wiring.
