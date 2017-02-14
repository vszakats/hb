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

# Verify if the last commit updated any HBDOC files. This requires
# a repository with a history of at least the last commit.
if git diff-index --name-only HEAD~1 | grep 'doc/en' > /dev/null; then

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
    | tar --strip-components 1 -x --exclude README.txt
  )

  # Make a clone of the GitHub Pages repo for publishing results
  url="https://github.com/${slug_doc_pages}.git"
  echo "! Cloning GitHub Pages repo '${url}'..."
  git clone --depth 2 "${url}" "${hbdoc_fmt}"

  # Generate docs
  ${_bin_hbdoc} -repr "-format=${hbdoc_fmt}"

  # Update origin
  (
    cd "${hbdoc_fmt}" || exit

    echo "! Updating GitHub Pages repo..."

    git remote rm origin
    (
      set +x
      readonly GITHUB_USER='vszakats'
      git remote add origin "https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com/${slug_doc_pages}.git"
      git config user.name 'hbdoc-bot'
      git config user.email "${GITHUB_USER}@users.noreply.github.com"
    )

    # Update repository
    git commit -a -m 'update hbdoc generated guide'
    git push

    echo "! Update finished."
  )
else
  echo '! upd_doc: No doc changes detected, skip updating GitHub Pages repo.'
fi
