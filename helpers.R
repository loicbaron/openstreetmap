normalize_minmax <- function(x, ...) {
    return((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
}

mkdirs <- function(fp) {
    if (!file.exists(fp)) {
        mkdirs(dirname(fp))
        dir.create(fp)
    }
}
