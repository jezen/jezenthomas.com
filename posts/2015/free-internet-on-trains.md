---
title: Free Internet on Trains
date: 2015-05-07
excerpt: A bash function for changing your MAC address on OSX which usually works around Internet access time limits on public WiFi hotspots.
tags: code, osx, bash
---

<span class="run-in"><span class="drop">O</span>n many public WiFi
hotspots</span>, free access is limited to a certain amount of time per 24 hour
period — usually an hour. In some cases, like on the cross-country trains in the
UK, you are allowed 15 minutes(!) of gratis Internet access before being asked
to pay an extortionate amount for continued surfing on a flaky-at-best
connection.

In many cases, these public hotspots identify your machine with its MAC address,
so the simplest workaround is to change your MAC address. If you’re running OSX,
you can use the following Bash function to switch to a new MAC address.
Sometimes the script reuses the previous address, so you may have to keep
running `remac` until a new address is output. Add this to your `.bashrc` or
perhaps an `.aliases` file that you source when you start your shell.

```bash
function remac {
  sudo /System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -z
  sudo ifconfig en0 ether $(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')
  sudo networksetup -detectnewhardware
  echo $(ifconfig en0 | grep ether)
}
```

*N.B.:* You will have to reconnect to the public hotspot after changing your MAC
address. This is because the `remac` function needs to restart your WLAN card in
order for the new MAC address to take effect.

Some may argue that this is stealing. I would argue that Internet access ought
to be a basic human right and charging for it like this is grossly unethical in
the first place. The government provides free access to public libraries; why
should the World’s largest and most well-connect library be any different.
