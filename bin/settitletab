#!/bin/zsh -f

# This is the successor to settab and settitle functions (as well as the chpwd function).
# It avoids the explicit definition and use of special functions such as
# chpwd, precmd, preexec, instead setting appropriate arrays. This is to prevent conflicts
# with user's prompt functions and so forth.

# This is designed to put by default the computer name and whole directory path
# into the title bar (and if availble) the $PWD and penultimate directory
# in the tab. Also enables transient display of a running command (eg vim).
# Currently works with iTerm (OSX) and konsole (KDE) tabs, and should work
# with any standard X-like term (and Apple's Terminal.app).


version="1.0.1"



###############################################################################

#  Created by William G. Scott.
#  Copyright (c) 2007. All rights reserved.


#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
#    USA
#
#    cf. URL:   http://www.fsf.org/licensing/licenses/gpl.html
#
###############################################################################


function set_title_tab {

    function settab   {

        # file settab  -- invoked only if iTerm or Konsole is running

        #  Set iterm window tab to current directory and penultimate directory if the
        #  shell process is running.  Truncate to leave the rightmost $rlength characters.
        #
        #  Use with functions settitle (to set iterm title bar to current directory)
        #  and chpwd


    if [[ $TERM_PROGRAM == iTerm.app && -z "$KONSOLE_DCOP_SESSION" ]];then

      # The $rlength variable prints only the 20 rightmost characters. Otherwise iTerm truncates
      # what appears in the tab from the left.


        # Chage the following to change the string that actually appears in the tab:

          tab_label="$PWD:h:t/$PWD:t"

          rlength="20"   # number of characters to appear before truncation from the left

                echo -ne "\e]1;${(l:rlength:)tab_label}\a"


    else

        # For KDE konsole tabs

        # Chage the following to change the string that actually appears in the tab:

          tab_label="$PWD:h:t/$PWD:t"

          rlength="20"   # number of characters to appear before truncation from the left

            # If we have a functioning KDE console, set the tab in the same way
            if [[ -n "$KONSOLE_DCOP_SESSION" && ( -x $(which dcop)  )  ]];then
                    dcop "$KONSOLE_DCOP_SESSION" renameSession "${(l:rlength:)tab_label}"
            else
                : # do nothing if tabs don't exist
            fi

    fi
  }

    function settitle   {
    # Function "settitle"  --  set the title of the iterm title bar. use with chpwd and settab

    # Change the following string to change what appears in the Title Bar label:


      title_lab=$HOST:r:r::$PWD
        # Prints the host name, two colons, absolute path for current directory

      # Change the title bar label dynamically:

      echo -ne "\e]2;[zsh]   $title_lab\a"
  }

  # Set tab and title bar dynamically using above-defined functions

    function title_tab_chpwd { settab ; settitle }

    # Now we need to run it:
      title_tab_chpwd

  # Set tab or title bar label transiently to the currently running command

  if [[ "$TERM_PROGRAM" == "iTerm.app" ]];then
    function title_tab_preexec {  echo -ne "\e]1; $(history $HISTCMD | cut -b7- ) \a"  }
    function title_tab_precmd  { settab }
  else
    function title_tab_preexec {  echo -ne "\e]2; $(history $HISTCMD | cut -b7- ) \a"  }
    function title_tab_precmd  { settitle }
  fi

  # Use reserved named arrays instead of special functions if the ZSH version is 4.3.4 or above

    typeset -ga preexec_functions
    preexec_functions+=title_tab_preexec

    typeset -ga precmd_functions
    precmd_functions+=title_tab_precmd

    typeset -ga chpwd_functions
    chpwd_functions+=title_tab_chpwd

  # Otherwise we need to do this for older versions of zsh:

    if [[ $ZSH_VERSION < 4.3.4 ]];then
      function preexec { $preexec_functions }
      function precmd  { $precmd_functions  }
      function  chpwd  { $chpwd_functions   }
    fi
}

####################

set_title_tab
