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

# Select the AppVeyor build for updating the docs
[ "${APPVEYOR}" = 'True' ] || exit 0

cd "$(dirname "$0")/.." || exit

# Test strings:
#   + doc/en/file.txt
#   + contrib/name/doc/pt_PR/file.txt
#   + contrib/hbdoc/file.ext
#   - src/contrib/hbdoc/file.ext
#   - contrib/hbdoc/po/file.po
#   - src/dir/file.ext
readonly hbdoc_file_mask='((^|\/)doc\/[a-zA-Z0-9_]+\/|^contrib\/hbdoc\/[a-z0-9_]+\.[a-z]+)'

# DEBUG
echo '---'
git diff-index --name-only HEAD~1
echo '---'
git diff-index --name-only HEAD~1 | grep -E "${hbdoc_file_mask}"
echo '---'

# Verify if the last commit updated any HBDOC files or the hbdoc tool itself.
# This requires a repository with a history of at least the last commit.
if git diff-index --name-only HEAD~1 \
   | grep -q -E "${hbdoc_file_mask}"; then

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

  # Prepare deploy key
  readonly GITHUB_DEPLOY_HB_DOC_KEY="$(realpath './package')/deploy_hb_doc.key"
  (
    set +x
    if [ -n "${GITHUB_DEPLOY_HB_DOC_PASS}" ]; then
      gpg --batch --passphrase "${GITHUB_DEPLOY_HB_DOC_PASS}" -o "${GITHUB_DEPLOY_HB_DOC_KEY}" -d "${GITHUB_DEPLOY_HB_DOC_KEY}.asc"
    fi
  )

  # Update origin

  if [ -n "${GITHUB_TOKEN}${GITHUB_DEPLOY_HB_DOC_PASS}" ]; then
  (
    cd "${hbdoc_fmt}" || exit

    echo "! Updating Reference Guide repository..."

    (
      set +x
      readonly GITHUB_USER='vszakats'
      git config user.name "${GITHUB_USER}-auto"
      git config user.email "${GITHUB_USER}@users.noreply.github.com"
      git remote rm origin
      if [ -f "${GITHUB_DEPLOY_HB_DOC_KEY}" ]; then
        chmod 600 "${GITHUB_DEPLOY_HB_DOC_KEY}"

        # Add verified result of `ssh-keyscan github.com` to `known_hosts`
        mkdir -p "${HOME}/.ssh" || true
        echo 'github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==' >> "${HOME}/.ssh/known_hosts"

        git remote add origin "git@github.com:${slug_doc_pages}.git"
        # Requires Git 2.10.0 (2016-09)
        git config core.sshCommand "ssh -o BatchMode=yes -o StrictHostKeyChecking=yes -i '${GITHUB_DEPLOY_HB_DOC_KEY}'"
      else
        git remote add origin "https://${GITHUB_USER}@github.com/${slug_doc_pages}.git"
        git config credential.helper store
        echo "https://${GITHUB_USER}:${GITHUB_TOKEN}@github.com" > "${HOME}/.git-credentials"
      fi
    )

    # Add all files (to force adding any new ones)
    git add .
    if TZ=UTC git commit -a -m "update content

Based on ${url_source}"; then
      git push origin master || true
    fi

    echo "! Update finished."
  )
  else
    echo '! upd_doc: Not master branch or credentials missing, skip updating docs.'
  fi
else
  echo '! upd_doc: No changes detected, skip regenerating docs.'
fi
