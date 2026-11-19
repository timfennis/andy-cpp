#!/usr/bin/env bash
# Brings the next pull request with auto-merge on up to date with master, so
# that pull requests merge one after another, each tested on the master it
# lands on.
#
# The pull requests with auto-merge on queue up in the order it was turned on.
# The first one that isn't failing or conflicting is the one in progress: if it
# is behind master, this rebases it onto master and runs the Rust checks on it;
# otherwise its checks are running or it is about to merge, and this waits for
# that. A pull request whose checks fail, or that conflicts with master, keeps
# its place but is passed over until a push fixes it.
#
# Needs GH_TOKEN and GITHUB_REPOSITORY. With DRY_RUN=1 it only says what it
# would do.
set -euo pipefail

owner=${GITHUB_REPOSITORY%/*}
name=${GITHUB_REPOSITORY#*/}

# The open pull requests into master with auto-merge on, in queue order.
queue() {
  # shellcheck disable=SC2016
  gh api graphql -F owner="$owner" -F name="$name" -f query='
    query($owner: String!, $name: String!) {
      repository(owner: $owner, name: $name) {
        pullRequests(states: OPEN, baseRefName: "master", first: 100) {
          nodes {
            id number isDraft isCrossRepository headRefName headRefOid mergeStateStatus
            autoMergeRequest { enabledAt }
            commits(last: 1) { nodes { commit { statusCheckRollup { state } } } }
          }
        }
      }
    }' --jq '
    [.data.repository.pullRequests.nodes[]
      | select(.autoMergeRequest != null and (.isDraft | not) and (.isCrossRepository | not))]
    | sort_by(.autoMergeRequest.enabledAt)[]
    | [.id, .number, .headRefName, .headRefOid, .mergeStateStatus,
       (.commits.nodes[0].commit.statusCheckRollup.state // "NONE")]
    | @tsv'
}

# GitHub works out whether a pull request is behind lazily, so right after a
# push to master it can still say UNKNOWN. Ask again until it knows.
for attempt in 1 2 3 4 5 6; do
  next=""
  pull_requests=$(queue)
  while IFS=$'\t' read -r id number branch head merge_state checks; do
    if [[ -z $id ]]; then
      continue
    elif [[ $checks == FAILURE || $checks == ERROR ]]; then
      echo "#$number: checks failed, passing over it"
    elif [[ $merge_state == DIRTY ]]; then
      echo "#$number: conflicts with master, passing over it"
    else
      next="$id"$'\t'"$number"$'\t'"$branch"$'\t'"$head"$'\t'"$merge_state"
      break
    fi
  done <<<"$pull_requests"

  if [[ -z $next ]]; then
    echo "Nothing to bring up to date"
    exit 0
  fi
  IFS=$'\t' read -r id number branch head merge_state <<<"$next"
  [[ $merge_state != UNKNOWN ]] && break
  echo "#$number: GitHub hasn't worked out whether it is behind master yet (attempt $attempt)"
  sleep 10
done

if [[ $merge_state != BEHIND ]]; then
  echo "#$number is in progress ($merge_state), waiting for it"
  exit 0
fi

echo "#$number is next and behind master: rebasing $branch onto master"
[[ ${DRY_RUN:-} == 1 ]] && exit 0

# shellcheck disable=SC2016
gh api graphql -F id="$id" -F head="$head" -f query='
  mutation($id: ID!, $head: GitObjectID!) {
    updatePullRequestBranch(input: {pullRequestId: $id, expectedHeadOid: $head, updateMethod: REBASE}) {
      pullRequest { number }
    }
  }' >/dev/null

# A push made with the workflow's token starts no workflows, so the checks
# don't run on the rebased branch by themselves. Wait for the rebase to land,
# then start them.
for _ in $(seq 30); do
  current=$(gh api "repos/$GITHUB_REPOSITORY/git/ref/heads/$branch" --jq .object.sha)
  [[ $current != "$head" ]] && break
  sleep 2
done
if [[ $current == "$head" ]]; then
  echo "The rebase of $branch hasn't landed" >&2
  exit 1
fi

gh workflow run rust.yml --repo "$GITHUB_REPOSITORY" --ref "$branch"
echo "Started the Rust checks on $branch at $current"
