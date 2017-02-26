#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Rebuild html docs and commit it to their repository.
#
# To be run after each source repository commit, where HBDOC sources are
# maintained. If `ng-hbdoc` repository gets an update (which stored 3rd
# party docs), it won't have an effect until the next core HBDOC update.

cd "$(dirname "$0")/.." || exit

_BRANCH="$1"

# DEBUG
echo '---'
git diff-index --name-only HEAD~1
echo '---'
git diff-index --name-only HEAD~1 | grep -E '((^|\/)doc\/[a-zA-Z0-9_]+\/|^contrib\/hbdoc\/[a-z0-9_]+\.[a-z]+)'
echo '---'

# Verify if the last commit updated any HBDOC files or the hbdoc tool itself.
# This requires a repository with a history of at least the last commit.
# Test strings:
#   + doc/en/file.txt
#   + contrib/name/doc/pt_PR/file.txt
#   + contrib/hbdoc/file.ext
#   - src/contrib/hbdoc/file.ext
#   - contrib/hbdoc/po/file.po
#   - src/dir/file.ext
if git diff-index --name-only HEAD~1 \
   | grep -q -E '((^|\/)doc\/[a-zA-Z0-9_]+\/|^contrib\/hbdoc\/[a-z0-9_]+\.[a-z]+)'; then

  case "$(uname)" in
    *_NT*)   readonly os='win';;
    Linux*)  readonly os='linux';;
    Darwin*) readonly os='mac';;
    *BSD)    readonly os='bsd';;
  esac

  if [ "${os}" = 'win' ]; then
    _bin_hbdoc="$(find bin -type f -name 'hbdoc.exe' | head -n 1)"
  else
    _bin_hbdoc="$(find bin -type f -name 'hbdoc' | head -n 1)"
  fi

  readonly url_source="$(git config remote.origin.url | sed -e 's/.git$//')/commit/$(git rev-parse --verify HEAD)"

  echo "! hbdoc binary: ${_bin_hbdoc}"

  # Constants

  readonly slug_3rd_hbdoc='harbour/ng-hbdoc'
  readonly slug_doc_pages='harbour/doc'
  readonly hbdoc_fmt='html'

  # Download and unpack 3rd party HBDOC files
  (
    cd contrib || exit
    url="https://github.com/${slug_3rd_hbdoc}/archive/master.tar.gz"
    echo "! Downloading 3rd party docs '${url}'..."
    curl -fsS -L --proto-redir =https "${url}" \
    | tar --strip-components 1 -zx --exclude README.txt
  )

  # Make a clone of the Reference Guide repository for publishing results
  url="https://github.com/${slug_doc_pages}.git"
  echo "! Cloning Reference Guide repository '${url}'..."
  git clone --depth 2 "${url}" "${hbdoc_fmt}"

  # Delete all files (to ensure that any file no longer generated will be
  # purged from the Reference Guide repository.)
  ( cd "${hbdoc_fmt}" && git rm -q -rf . )

  # Generate docs
  ${_bin_hbdoc} -v0 -repr "-format=${hbdoc_fmt}" || exit

  # Update origin

  if [ "${_BRANCH#*master*}" != "${_BRANCH}" ] && \
     [ -n "${GITHUB_TOKEN}" ]; then
  (
    cd "${hbdoc_fmt}" || exit

    echo "! Updating Reference Guide repository..."

    git remote rm origin
    (
      set +x
      readonly GITHUB_USER='vszakats'
      git remote add origin "https://${GITHUB_USER}@github.com/${slug_doc_pages}.git"
      git config user.name "${GITHUB_USER}-auto"
      git config user.email "${GITHUB_USER}@users.noreply.github.com"
      git config credential.helper store
      [ "${os}" = 'win' ] && export HOME="${USERPROFILE}"
      echo "https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com" > "${HOME}/.git-credentials"
    )

    # Add all files (to force adding any new ones)
    git add .
    git commit -a -m "update content

Based on ${url_source}"
    git push origin master

    echo "! Update finished."
  )
  else
    echo '! upd_doc: Not master branch or credentials missing, skip updating docs.'
  fi
else
  echo '! upd_doc: No changes detected, skip regenerating docs.'
fi
