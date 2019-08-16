#!/bin/bash

# Utility functions

# Launch Daemons

# Disable Apple Push Notification Service daemon
# https://apple.stackexchange.com/questions/92214/how-to-disable-apple-push-notification-service-apsd-on-os-x-10-8
sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.apsd.plist

# Disable CalendarAgent
launchctl unload -w /System/Library/LaunchAgents/com.apple.CalendarAgent.plist

# Disable NetBIOS daemon (netbiosd)
sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.netbiosd.plist

# Disable Location Services (locationd)
sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.locationd.plist

# Disable Notification Center
# https://apple.stackexchange.com/questions/106149/how-do-i-permanently-disable-notification-center-in-mavericks
sudo launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist

# Disable QuickLook
# https://superuser.com/questions/617658/quicklooksatellite-mac-os-high-cpu-use
sudo launchctl unload -w /System/Library/LaunchAgents/com.apple.quicklook.*

# Disable Spotlight
# http://osxdaily.com/2011/12/10/disable-or-enable-spotlight-in-mac-os-x-lion/
sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist

# Disabling Maverick's Unicast ARP Cache Validation Script (thanks, MMV!)
if [[  $(sw_vers -productVersion | grep '10.9')  ]]
then
   if [[ -f /etc/sysctl.conf ]]
   then
      if grep 'unicast' /etc/sysctl.conf > /dev/null 2>&1
      then
         echo "PATCH WAS PREVIOUSLY ENABLED"
         exit
      fi
    fi
         sudo sysctl -w net.link.ether.inet.arp_unicast_lim=0  > /dev/null 2>&1
         echo "net.link.ether.inet.arp_unicast_lim=0" | sudo tee -a /etc/sysctl.conf  > /dev/null 2>&1
         sudo chown root:wheel /etc/sysctl.conf
         sudo chmod 644 /etc/sysctl.conf
         echo "PATCH ENABLED" 
fi

# Disable Bonjour Script (thanks MMV!)
if [[  $(sw_vers) ]]
then
    # MAKES SURE WE ARE RUNNING 10.6 -> 10.9 or 10.10.4+
    if [[  $(sw_vers -productVersion | grep '10.[6-9]') ]] || [[  $(sw_vers -productVersion | grep '10.10.[4-5]') ]]
    then
        # CHECKS FOR FLAG IN CURRENT PLIST FILE
        if [[ $(sudo /usr/libexec/PlistBuddy -c Print /System/Library/LaunchDaemons/com.apple.mDNSResponder.plist | grep 'NoMulticast') ]]
        then
            echo "MULTICAST DISABLED, NO CHANGES MADE"
        else
            sudo /usr/libexec/PlistBuddy -c "Add :ProgramArguments: string -NoMulticastAdvertisements" /System/Library/LaunchDaemons/com.apple.mDNSResponder.plist
            echo "MULTICAST DISABLED (OS X 10.6-10.9 or 10.10.4+), PLEASE REBOOT"
        fi
        exit
    else
    echo  "OS X 10.6 - 10.9 or 10.10.4+ NOT DETECTED, NO CHANGES HAVE BEEN MADE YET"
    echo  "CHECKING FOR OS X 10.10.0 to 10.10.3 ..."
        if [[  $(sw_vers -productVersion | grep '10.10[.0-3]') ]]
            then
                    # CHECKS FOR FLAG IN CURRENT PLIST FILE
            if [[ $(sudo /usr/libexec/PlistBuddy -c Print /System/Library/LaunchDaemons/com.apple.discoveryd.plist | grep 'no-multicast') ]]
                    then
                echo "MULTICAST DISABLED, NO CHANGES MADE"
                    else
                            sudo /usr/libexec/PlistBuddy -c "Add :ProgramArguments: string --no-multicast" /System/Library/LaunchDaemons/com.apple.discoveryd.plist
                            echo "MULTICAST DISABLED (OSX 10.10), PLEASE REBOOT"
                    fi
                    exit
        else
        echo "OS X 10.10 NOT DETECTED, NO CHANGES HAVE BEEN MADE"
        fi
    fi
else
echo "SORRY, OS X NOT DETECTED - NO CHANGES MADE"
fi
exit

# Launch Agents

DISABLE_DIR=/System/Library/LaunchAgents.bak
sudo mkdir -p ${DISABLE_DIR}

# Disable Game Center daemon (gamed)
sudo mv /System/Library/LaunchAgents/com.apple.gamed.plist ${DISABLE_DIR}

# Disable Airplay Mirroring
# http://www.ehcho.com/guide/disable-airplay-mirroring/
sudo mv /System/Library/LaunchAgents/com.apple.AirPlayUIAgent.plist ${DISABLE_DIR}

