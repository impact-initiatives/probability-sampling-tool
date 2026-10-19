#!/usr/bin/env bash
# Check that a release tag is the version of the app.
#
# Usage: .github/scripts/check-tag.sh <tag> [constants file]
#
# The tag is "v" followed by the version, in calendar versioning (vYYYY.N.N), and
# the version is APP_VERSION in R/constants.R. Used by the CI when a tag is pushed,
# and meant for the deployment to production too.
set -euo pipefail

tag="${1:?usage: check-tag.sh <tag> [constants file]}"
constants="${2:-R/constants.R}"

if [[ ! "$tag" =~ ^v[0-9]{4}\.[0-9]+\.[0-9]+$ ]]; then
  echo "::error::The tag '$tag' is not a version tag: expected vYYYY.N.N, for example v2026.1.0"
  exit 1
fi

version=$(sed -n 's/^APP_VERSION <- "\(.*\)"[[:space:]]*$/\1/p' "$constants")
if [[ -z "$version" ]]; then
  echo "::error::APP_VERSION was not found in $constants"
  exit 1
fi

if [[ "$tag" != "v$version" ]]; then
  echo "::error::The tag is '$tag' but APP_VERSION is '$version' ($constants): change one of them"
  exit 1
fi

echo "The tag $tag is the version of the app."
