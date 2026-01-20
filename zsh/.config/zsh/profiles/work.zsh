# Here you can add work specific exports, aliases and functions that are not
# recommended to be versioned.


export GOPRIVATE=gogs.marathon.mesos
export GOINSECURE=gogs.marathon.mesos

export _ZO_EXCLUDE_DIRS=$HOME:/home/ivanvd/Projects/_gogs.mesos/*:/home/ivanvd/Projects/gogs.mesos/*

function pexec() {
	# same as `basename $PWD` but probably faster;
	# it returns the last part of CWD
	CONTAINER="${PWD##*/}"

	command podman container exec -it "$CONTAINER" "$@"
}
