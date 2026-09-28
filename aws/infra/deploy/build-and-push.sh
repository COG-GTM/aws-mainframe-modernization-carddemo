#!/usr/bin/env bash
# Builds the CardDemo images and pushes them to the ECR repositories created by the Services/Batch stacks.
# Usage: ENV_NAME=dev AWS_REGION=us-east-1 TAG=latest ./build-and-push.sh [online-services|frontend|batch|etl ...]
set -euo pipefail
ENV_NAME="${ENV_NAME:-dev}"
AWS_REGION="${AWS_REGION:-us-east-1}"
TAG="${TAG:-latest}"
AWS_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
ACCOUNT="$(aws sts get-caller-identity --query Account --output text)"
REGISTRY="${ACCOUNT}.dkr.ecr.${AWS_REGION}.amazonaws.com"

DEFAULT_SERVICES_CONTEXT="${AWS_DIR}/services"
[ -f "${DEFAULT_SERVICES_CONTEXT}/Dockerfile" ] || DEFAULT_SERVICES_CONTEXT="${AWS_DIR}/online-services"
declare -A CONTEXTS=(
  [online-services]="${SERVICES_CONTEXT:-${DEFAULT_SERVICES_CONTEXT}}"
  [frontend]="${FRONTEND_CONTEXT:-${AWS_DIR}/frontend}"
  [batch]="${BATCH_CONTEXT:-${AWS_DIR}/batch}"
  [etl]="${ETL_CONTEXT:-${AWS_DIR}/etl}"
)
COMPONENTS=("$@")
[ ${#COMPONENTS[@]} -eq 0 ] && COMPONENTS=(online-services frontend batch etl)

aws ecr get-login-password --region "$AWS_REGION" | docker login --username AWS --password-stdin "$REGISTRY"
for c in "${COMPONENTS[@]}"; do
  ctx="${CONTEXTS[$c]:?unknown component $c}"
  if [ ! -f "$ctx/Dockerfile" ]; then echo "error: no Dockerfile for $c in $ctx (set the *_CONTEXT override)" >&2; exit 1; fi
  image="${REGISTRY}/carddemo-${ENV_NAME}-${c}:${TAG}"
  docker build --platform linux/amd64 -t "$image" "$ctx"
  docker push "$image"
done
