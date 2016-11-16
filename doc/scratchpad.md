Scratchpad
==========

Below are random notes that have not yet went into documentation.

Regarding Q22 in test-tpch taking too long to run:

> use 'showBackendCodeQ optBU naturalPgCodeGen q22Default' to check it out

> (2) Modify your PostgreSQL configuration to allow more memory being
> used. Most relevant are the shared_buffers and work_mem parameters. The
> default settings are overly conservative.
