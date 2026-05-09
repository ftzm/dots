#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage: $(basename "$0") -n NAMESPACE -s SECRET_NAME KEY=VALUE [KEY=VALUE...]

Create a SealedSecret and output Jsonnet code.

Options:
  -n NAMESPACE       Kubernetes namespace for the secret
  -s SECRET_NAME     Name of the secret
  -c CONTROLLER      Sealed secrets controller name (default: sealed-secrets)
  -C CONTROLLER_NS   Sealed secrets controller namespace (default: sealed-secrets)
  -o OUTPUT_FILE     Output file (default: <secret-name>.sealed.jsonnet)
  -h                 Show this help message

Example:
  $(basename "$0") -n monitoring -s alertmanager-config smtp-password=secret123

EOF
  exit 1
}

namespace=""
secret_name=""
controller_name="sealed-secrets"
controller_namespace="sealed-secrets"
output_file=""

while getopts "n:s:c:C:o:h" opt; do
  case $opt in
    n) namespace="$OPTARG" ;;
    s) secret_name="$OPTARG" ;;
    c) controller_name="$OPTARG" ;;
    C) controller_namespace="$OPTARG" ;;
    o) output_file="$OPTARG" ;;
    h) usage ;;
    *) usage ;;
  esac
done
shift $((OPTIND - 1))

if [[ -z "$namespace" || -z "$secret_name" || $# -eq 0 ]]; then
  echo "Error: namespace, secret name, and at least one KEY=VALUE pair required" >&2
  usage
fi

# Default output file if not specified
if [[ -z "$output_file" ]]; then
  output_file="${secret_name}.sealed.jsonnet"
fi

# Build the secret data section
data_args=()
for kv in "$@"; do
  if [[ "$kv" != *"="* ]]; then
    echo "Error: Invalid KEY=VALUE pair: $kv" >&2
    exit 1
  fi
  key="${kv%%=*}"
  value="${kv#*=}"
  data_args+=(--from-literal="$key=$value")
done

# Create secret, seal it, and transform with jsonnet
sealed_json=$(kubectl create secret generic "$secret_name" \
  --namespace="$namespace" \
  --dry-run=client \
  -o yaml \
  "${data_args[@]}" \
  | kubeseal --format json \
      --controller-name="$controller_name" \
      --controller-namespace="$controller_namespace")

jsonnet --ext-code "sealed=$sealed_json" -e '
  local sealed = std.extVar("sealed");
  {
    apiVersion: "bitnami.com/v1alpha1",
    kind: "SealedSecret",
    metadata: sealed.metadata,
    spec: {
      encryptedData: sealed.spec.encryptedData,
      template: {
        metadata: sealed.metadata,
      },
    },
  }
' > "$output_file"

echo "Wrote sealed secret to $output_file"
