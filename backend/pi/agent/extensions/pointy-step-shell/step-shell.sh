set -euo pipefail

step=$1
command=$2
inputs_expression=$3
worktree=$POINTY_AGENT_WORKTREE
installable="path:$worktree#pointy.steps.\"$step\""

inputs=$(nix eval --raw "$installable" --apply "$inputs_expression")

dependency_links=()
packages=()
missing_steps=()
while read -r kind dependency_step path; do
	case $kind in
	dependency)
		if nix path-info "$path" >/dev/null 2>&1; then
			dependency_links+=("$dependency_step=$path")
		else
			missing_steps+=("$dependency_step")
		fi
		;;
	package) packages+=("$path") ;;
	esac
done < <(sort -u <<<"$inputs")

if ((${#missing_steps[@]})); then
	printf 'Step %s has no output yet. step_shell never builds dependencies: ask the user to run it.\n' "${missing_steps[@]}" >&2
	exit 1
fi

environment=$(nix print-dev-env "$installable")

directory=$HOME/steps/$step
mkdir -p "$directory/out" "$HOME/tmp"
find "$directory" -mindepth 1 -maxdepth 1 -type l -delete
if [ -d "$worktree/srcFiles/$step" ]; then
	find "$worktree/srcFiles/$step" -mindepth 1 -maxdepth 1 -exec ln -s {} "$directory" \;
fi
for link in "${dependency_links[@]}"; do
	ln -sfn "${link#*=}" "$directory/${link%%=*}"
done

set +euo pipefail
export TMPDIR=$HOME/tmp
eval "$environment"
for package in "${packages[@]}"; do
	PATH=$package/bin:$PATH
done
cd "$directory" || exit
bash -c "$command"
status=$?
rm -rf "$NIX_BUILD_TOP"
exit "$status"
