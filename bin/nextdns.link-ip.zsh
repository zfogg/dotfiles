#!/usr/bin/env zsh

NEXT_DNS_IP="$(/usr/bin/curl -s 'https://link-ip.nextdns.io/a6e983/5dfeb043a29e3815')"

LOG_TAG="nextdns-link-ip"

if [[ $? == 0 ]]; then
    logger -t "$LOG_TAG" -i  "NextDNS IP updated: $NEXT_DNS_IP"
else
    logger -t "$LOG_TAG" -is "NextDNS IP update FAILED"
fi
