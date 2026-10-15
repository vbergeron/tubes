package tubes

/** Inside a pipeline step, `$` is the schema of the rows flowing into that step. */
inline transparent def $(using schema: Schema[?]): schema.type = schema
