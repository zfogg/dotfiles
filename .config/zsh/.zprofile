#!/usr/bin/env zsh
# vim :set syntax=zsh:
# vim :set filetype=zsh:



# Are we in the bottle?
#if [[ ! -v INSIDE_GENIE && -x /usr/bin/genie ]]; then
#  read -t 3 "yn?* Preparing to enter genie bottle (in 3s); abort?"
#  echo
#
#  if [[ $yn != "y" ]]; then
#    echo "Starting genie:"
#    exec /usr/bin/genie -s
#  fi
#fi

if [[ ! -v INSIDE_GENIE && -x /usr/bin/genie ]]; then
  exec /usr/bin/genie -s
  #exec /usr/bin/genie -l
  #exec /usr/bin/genie -i
  if [[ -v INSIDE_GENIE ]]; then
    echo -n " * Waiting for systemd (user)..."
    until systemctl --user is-system-running &>/dev/null; do
      sleep 1
      echo -n "."
    done
    echo
  fi
fi
