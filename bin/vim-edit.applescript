#!/usr/bin/env osascript
# vim: filetype=applescript:


on run argv
    try
        set TargetFile to quoted form of (item 1 of argv)
    on error emsg number enum
        if enum is in {-1728, -1721} then
            error emsg number -1721
        end if
    end try

    tell application "iTerm"
        set VimSessionName to "(nvim)"
        set VimSessions to {}

        # find all terminal sessions that are editing files
        repeat with iTermWindow in windows
            tell iTermWindow
                repeat with iTermTab in tabs
                    tell iTermTab
                        repeat with iTermSession in sessions
                            tell iTermSession
                                if name of iTermSession contains VimSessionName then
                                    copy iTermSession to the end of VimSessions
                                end if
                            end tell # session
                        end repeat # sessions
                    end tell # tab
                end repeat # tabs
            end tell # window
        end repeat # windows

        # find any session already editing the target file
        repeat with VimSession in VimSessions
            tell VimSession to log (get name)
        end repeat

    end tell # iTerm2
end run
