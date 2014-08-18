# Rescoyl Checks

This is a Docker Registry conformance client written to validate a registry
implementation against the registry protocol
[specification](https://docs.docker.com/reference/api/hub_registry_spec/).

The client itself is actually tested against the [official Docker
registry](https://github.com/docker/docker-registry) and the official registry
behavior is more important that the specification which can be outdated.

The client is written primarily to ensure the [Rescoyl
registry](https://github.com/docker/docker-registry) behavior matches with the
official registry but can be used to validate other implementions.

It is also possible to use this package as a library to write normal Docker
registry clients.

There is also the beginning of bindings to the Docker Remote API (they should
move to their own library at some point).
