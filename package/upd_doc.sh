#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2017 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# Rebuild html docs and commit it to their repository.
#
# To be run after each Harbour repository commit, where Harbour HBDOC sources
# are maintained. If `ng-hbdoc` repository gets an update, it won't have an
# effect until a subsequent Harbour repository update is made.

cd "$(dirname "$0")/.." || exit

[ -n "${GITHUB_TOKEN}" ] || exit

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
   | grep -E '((^|\/)doc\/[a-zA-Z0-9_]+\/|^contrib\/hbdoc\/[a-z0-9_]+\.[a-z]+)' > /dev/null; then

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

  # Make a clone of the GitHub Pages repository for publishing results
  url="https://github.com/${slug_doc_pages}.git"
  echo "! Cloning GitHub Pages repository '${url}'..."
  git clone --depth 2 "${url}" "${hbdoc_fmt}"

  # Generate docs
  ${_bin_hbdoc} -v0 -repr "-format=${hbdoc_fmt}"

  # Update origin
  (
    cd "${hbdoc_fmt}" || exit

    echo "! Updating GitHub Pages repository..."

    git remote rm origin
    (
      set +x
      readonly GITHUB_USER='vszakats'
      git remote add origin "https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com/${slug_doc_pages}.git"
      git config user.name "${GITHUB_USER}-auto"
      git config user.email "${GITHUB_USER}@users.noreply.github.com"
    )

    # Update repository
    git commit -a -m "update content

Based on ${url_source}"
    git push origin master

    echo "! Update finished."
  )
else
  echo '! upd_doc: No doc changes detected, skip updating GitHub Pages repository.'
fi
