_sup()
{
    local cur prev opts base
    kazoopath="/opt/kazoo"
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    #
    # Find all whapps that contain maintenance commands
    #
    opts=`find $kazoopath/*/*/src/*maintenance.erl |sed 's/.*\///' |sed 's/.erl$//' | xargs`

    #
    #  Complete the arguments
    #
    case "${prev}" in
	*_maintenance)
            local commands=$(grep export $kazoopath/*/*/src/${prev}.erl |sed 's/.*(\[//' | sed 's/]).//')
	        COMPREPLY=( $(compgen -W "${commands}" -- ${cur}) )
            return 0
            ;;
	*)
	;;
    esac

   COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
   return 0
}
complete -F _sup sup
