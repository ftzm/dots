# Render all environments
render-all: render-lab

# Render lab environment
render-lab:
    rm -rf manifests/lab/*
    tk export manifests/lab/ environments/lab --format '{{ "{{" }}.metadata.name{{ "}}" }}-{{ "{{" }}.kind | lower{{ "}}" }}' --skip-manifest
    cp environments/lab/secrets/*.enc.yaml manifests/lab/

# Initialize jsonnet-bundler dependencies
jb-install:
    jb install

# Update jsonnet-bundler dependencies
jb-update:
    jb update

# Show diff of what would change in lab
diff-lab:
    tk diff environments/lab

# Generate renovate.json from chartfile.yaml
generate-renovate:
    jsonnet renovate.jsonnet > renovate.json
    renovate-config-validator

# Test renovate config locally (dry-run)
test-renovate:
    RENOVATE_CONFIG_FILE=renovate.json renovate --platform=local --dry-run=full --onboarding=false
