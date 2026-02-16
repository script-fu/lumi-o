---
title: "GitLab CI"
type: docs
url: "hub/technical-guides/folder/GitLab-CI"
---

Continuous Integration (CI) is a way to automatically test, build, and validate your code whenever changes are made.

**GitLab** provides built-in CI/CD features through its `.gitlab-ci.yml` file. This file, placed in the root of your repository, tells GitLab how to build and test your project. It defines stages and scripts that are run in a clean environment every time changes are pushed.

This document outlines how Lumi's GitLab CI/CD pipeline works, including the role of the `.gitlab-ci.yml` file, shell scripts, and external tools like Meson and Ninja.

For detailed technical documentation of the Lumi CI build process, see [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md) in the repository.

## GitLab CI/CD Basics

The CI is controlled by a file named `.gitlab-ci.yml`. This file defines:

- **Stages**: Ordered groups of jobs (e.g., `build-this`, `build-that`, `package-up`)
- **Jobs**: Individual tasks to run within each stage
- **Scripts**: Shell commands executed for each job
- **Runners**: Computers that GitLab uses to run jobs defined in the pipeline.

In Lumi, the pipeline stages are:

- `dependencies`
- `build lumi`
- `appimage`

## Container-Based Builds

The Lumi pipeline uses containerization for consistent builds:

1. **Creating the Build Container**: The first stage uses Buildah to create a Docker image with all dependencies
2. **Using the Container**: Subsequent stages run inside this container, ensuring consistent environment
3. **Reproducible Builds**: Container isolation guarantees the same results across different runners

This approach ensures that builds work the same way across any GitLab runner and provides a controlled environment for complex build processes.

## Role of Shell Scripts

Jobs in `.gitlab-ci.yml` typically invoke shell commands directly. Complex operations are often moved into separate scripts stored in the repository.

The Lumi CI uses modular shell scripts to organize build logic:

**Example of script invocation:**
```yaml
script:
  - bash build/linux/appimage/lumi-goappimage.sh 2>&1 | tee appimage_creation.log
```

**Benefits of this approach:**
- **Clean YAML**: Keeps the `.gitlab-ci.yml` file focused on job structure
- **Maintainability**: Complex logic is easier to debug and modify in shell scripts
- **Reusability**: Scripts can be used in different contexts or environments
- **Modularity**: Different aspects of the build can be separated into focused scripts

This keeps the CI configuration clean while allowing sophisticated build processes.

## Integration with Build Systems

Lumi uses **Meson** and **Ninja** to prepare and then build the code.

For example:

```
script:
  - meson setup _build-${CI_RUNNER_TAG} -Dprefix="${LUMI_PREFIX}"
  - ninja -C _build-${CI_RUNNER_TAG}
  - ninja -C _build-${CI_RUNNER_TAG} install
```

Here:

- `meson setup` prepares the build directory and generates `build.ninja`
- `ninja` runs the build commands as defined

## Meson Build System Structure

The **Meson** build system uses a root `meson.build` file placed at the project’s root directory. This file defines the top-level build configuration and entry point for the build process.

- The root `meson.build` is typically located in the same directory as `.gitlab-ci.yml`
- From there, it **cascades recursively** into subdirectories, each of which may have its own `meson.build` file
- These subdirectory files define targets, sources, dependencies, and build instructions relevant to that directory

## Environment Variables

Key variables in the Lumi pipeline include:

```yaml
variables:
  DEBIAN_FRONTEND: "noninteractive"  # Prevents interactive prompts
  DEB_VERSION: "trixie"              # Debian version for consistency
  CI_RUNNER_TAG: "x86_64"            # Architecture specification
```

**Job-specific variables:**
```yaml
build-lumi:
  variables:
    COMPILER: "clang"                                           # Compiler selection
    LINKER: "lld"                                               # Linker selection
    LUMI_PREFIX: "${CI_PROJECT_DIR}/_install-${CI_RUNNER_TAG}"  # Installation path
    DEPS_PREFIX: "/opt/lumi-deps"                               # Prebuilt dependency prefix
    MESON_OPTIONS: "-Dpkgconfig.relocatable=true -Drelocatable-bundle=yes"  # Build configuration
```

These variables control build behavior and ensure consistency across different stages and runners.

## Example Structure

```
project-root/
├── .gitlab-ci.yml
├── meson.build              <-- Root Meson file
├── src/
│   ├── meson.build          <-- Subdirectory Meson file
│   └── some_source.c
├── data/
│   ├── meson.build
│   └── icons/
```

In this structure:

- The root `meson.build` file configures the overall build environment
- Subdirectory `meson.build` files handle compilation details for specific components or modules
- This hierarchical layout keeps build logic modular and maintainable

## Artifacts Between Stages

Artifacts are files generated by jobs that are needed in subsequent stages:

```yaml
build-lumi:
  # ...job configuration...
  artifacts:
    paths:
      - "${LUMI_PREFIX}/"      # Installation files
      - _build-${CI_RUNNER_TAG}/meson-logs/meson-log.txt  # Build logs
```

## Pipeline Stages and Dependencies

The Lumi pipeline consists of three main stages:

1. **Dependencies**: Creates a containerized build environment with all required tools and libraries
2. **Build Lumi**: Compiles Lumi using Meson and Ninja in the prepared environment
3. **AppImage**: Packages the built application into a distributable AppImage format

**Stage Dependencies:**
```yaml
build-lumi:
  needs: [deps-debian]  # Waits for dependency container

lumi-appimage:
  needs: [build-lumi] # Waits for application build
```

Each stage runs only after its dependencies complete successfully, ensuring proper build order and artifact availability.

## Current Job Names

The Lumi `.gitlab-ci.yml` currently defines these job names:

- `deps-debian`
- `build-lumi`
- `lumi-appimage`

## Summary

- `.gitlab-ci.yml` defines the structure and logic of the pipeline
- Jobs contain shell commands or external scripts
- Tools like Meson and Ninja are used inside jobs as part of the build process

Lumi uses GitLab CI to automatically build its AppImage for Debian-based platforms. The pipeline builds dependencies, compiles Lumi, and then packages an AppImage.

For source-level details, use:

- `.gitlab-ci.yml` in the Lumi repository root
- `build/linux/appimage/lumi-goappimage.sh`
- `build/linux/appimage/README-CI.md`

For comprehensive technical details about the Lumi CI build process, including environment setup, script architecture, and troubleshooting, refer to [README-CI.md](https://gitlab.gnome.org/pixelmixer/lumi/-/blob/main/build/linux/appimage/README-CI.md).
