---
title: Deploying A Haskell Web Service With Nix
date: 2016-02-01
excerpt: The entire process from zero to “Hello, Haskell!” running on a cheap Amazon EC2 instance.
tags: haskell, operations
---

<span class="run-in"><span class="drop">T</span>here are several proposed
solutions</span> to the problem of unceremoniously deploying a Haskell web
service.

Docker. Keter. Cabal. Halcyon. Stackage. Haskell-on-Heroku.

I have tried them all, and for one reason or another, none of them seemed to
work for me, save for Nix and NixOps. What follows is a guide to getting started
with Haskell on Nix.

My development machine is a MacBook Air running OSX 10.10, which makes
deployment of a compiled Haskell binary to a Linux server slightly more
complicated. If you're running Linux, your deployment process may run more
quickly than what is described in this article. I've done my best to time all
the installation, download, and compilation steps, though the times are
approximate and will vary depending on network latency and machine performance.

<div id="toc"></div>

1. [Installation]
2. [Project Setup]
    1. [The Directory Structure]
    2. [A Simple Haskell Application]
    3. [A Simple Haskell Web Service]
    4. [Unfree Licenses]
3. [Deployment]
    1. [The Application Build Expression]
    2. [The Default File]
    3. [Services Configuration]
    4. [Physical Network Configuration]
    4. [Configuring Amazon EC2]
    5. [Deploying To An EC2 Instance]
    6. [Updating The Application]
4. [Errata]
5. [References]

## Installation

Installation is the process which takes the most time, but you can typically be
up and running in about 15 minutes, depending on the speed of your Internet
connection.

There is only one prerequisite; you will need to have the Xcode Command Line
Tools installed. You do *not* need to have the Haskell platform installed.

Since it runs over the network — and the network is inherently unreliable — the
installation is prone to failure. This shouldn't matter though, because Nix is
designed in such a way that most operations don't rely on state. If anything
fails, you can normally just rerun the command.

Run the following commands in succession, taking care to change `~/.zshrc` to
reflect the “run commands” file of your shell if necessary.

```sh
# Install Nix (about 30 seconds)
curl https://nixos.org/nix/install | sh

# Load Nix profile on login
# Change this file to reflect the shell you use
echo "\n. $HOME/.nix-profile/etc/profile.d/nix.sh" | tee -a ~/.zshrc

# Reload the shell
exec $SHELL -l

# I'm not sure why this is needed, but if you don't do it then Nix complains
# that the packages can't be found
nix-channel --update

# Install ghc, cabal2nix and cabal-install (about 10 minutes)
nix-env -iA nixpkgs.haskellPackages.cabal2nix \
            nixpkgs.haskellPackages.cabal-install \
            nixpkgs.haskellPackages.ghc
```

## Project Setup

I'll run through setting up an example application, first by describing the
directory structure, and then by listing the commands needed to get from zero to
*Hello, Haskell!*. For lack of a better name, I will call the application
*FestHest*.

### The Directory Structure

Here's the directory structure. We have a `festhest` directory nested inside of
another `festhest` directory; the thinking being that the configuration files
necessary for deployment to AWS live in the top-level directory, and everything
to do with your Haskell application lives in the `festhest/festhest` directory.

```sh
festhest
└── festhest
    ├── dist
    └── src
```

You don't need to manually create the `dist` or `src` directories; Cabal will
handle that for us in the next step.

### A Simple Haskell Application

Let's create our directories, and begin scaffolding with Cabal. The
Cabal-Install tool will interactively assist with creating a our project
scaffolding.

Most options can be left with default values. The options I choose for the
multiple-choice questions are as follows:

- Please choose a license: GPL-3
- Project category: Web
- What does the package build: Executable
- What is the main module of the executable: Main.hs
- What base language is the package written in: Haskell2010
- Source directory: src

```sh
mkdir -p festhest/festhest
cd $_
cabal init
```

Now we can write our first Haskell source file.

```haskell
-- | ~/festhest/festhest/src/Main.hs
module Main (
  main
) where

main = do
  print "Hello, Haskell!"
```

Back in the shell, we try compiling our Haskell code with Cabal. We can access
Cabal through the Nix shell, but first we need to create a configuration file
for the Nix shell, and then start the shell.

```sh
cabal2nix --shell . > shell.nix
nix-shell
```

Now that we've dropped into the Nix shell, we can use Cabal to first configure
and then run our application's compilation. Cabal will download and install some
dependencies; on my machine this takes around 30 seconds.

```sh
cabal configure
cabal run
```

You should see the Haskell code being compiled by GHC. If you see your `Hello,
Haskell!` message printed out, you know it has successfully compiled.

### A Simple Haskell Web Service

Having a program output a message is all very well, but we want this application
to serve requests over the Internet. We'll change our `Main.hs` source file so
it starts a web server and responds to a route.

```haskell
-- | ~/festhest/festhest/src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main (
  main
) where

import Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes| / HomeR GET |]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet| <h1>Hello, Haskell! |]

main = warp 80 App
```
The above code snippet is a bare-bones Yesod app. In order for us to have access
to Yesod, we need to add it to our Cabal file under `build-depends`, a few lines
up from the bottom.

```sh
-- | ~/festhest/festhest/festhest.cabal

name:                festhest
version:             0.1.0.0
synopsis:            Nothing is more fun than a festlig hest.
-- description:
homepage:            http://jezenthomas.com
license:             GPL-3
license-file:        LICENSE
author:              Jezen Thomas
maintainer:          jezen@jezenthomas.com
-- copyright:
category:            Web
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.10

executable festhest
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.8 && <4.9, yesod
  hs-source-dirs:      src
  default-language:    Haskell2010
```

Run `cabal2nix --shell . > shell.nix` again, and then drop into the Nix shell
with `nix-shell`. Nix will begin to download all of the project's new
dependencies (which come from Yesod). This process takes a little less than six
minutes on my machine.

If you try running `cabal run` from within the Nix shell, it should fail and
tell you that you need sudo privileges to serve the application on port 80.
Simple enough to fix; we just do `sudo cabal run` instead.

With the application running, we can point our browser to `http://localhost/`
and we should see a simple page containing our “Hello, Haskell!” message.

### Unfree Licenses

By default, Nix won't build any project that doesn't use a “free” license, like
the GNU General Public License. Apparently this setting can be toggled in
`~/.nixpkgs/config.nix`, but I don't have that file so I set an environment
variable instead (if I'm building without a free license).

```sh
NIXPKGS_ALLOW_UNFREE=1
```

This can either be prepended to any command that needs it (like `nix-shell`), or
exported in a `.env` file and enabled with something like [autoenv][autoenv].

## Deployment

Now that we have a working application, it's time to ship it to an Amazon EC2
(Elastic Compute Cloud) instance. We need to first produce a few different
configuration files to tell both Nix and NixOps how to build and deploy the
application.

### The Application Build Expression

We need a file that describes the build environment described in our cabal file
that Nix can understand. We can easily generate this file with the `cabal2nix`
tool that we installed earlier. The tool takes a project directory containing a
cabal file, so it's easiest to run the command from the project directory.

```sh
cd ~/festhest/festhest
cabal2nix . > festhest.nix
```

### The Default File

The default file contains some Nix boilerplate that calls our application build
expression with the packages we need to build the Haskell source code. The GHC
(Glasgow Haskell Compiler) version is important here, because if the version you
specify is not cached on Nix's servers, you'll spend several hours downloading
and compiling Haskell packages. This file should live in the project directory,
adjacent to the cabal file and the application build expression.

```sh
# ~/festhest/festhest/default.nix
{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
pkgs.haskell.packages.${compiler}.callPackage ./festhest.nix { }
```

### Services Configuration

This services configuration file declares things like which TCP ports are
allowed through the firewall, the absolute path of our application to systemd,
and that the webserver should start after the network has come up. NixOs by
default only accepts traffic on port 22 (SSH), so we need to add port 80 there
to make requests to our web service.

```sh
# ~/festhest/festhest-configuration.nix
{
  network.description = "festhest";

  festhest =
    { config, pkgs, ... }: let
      festhest = import ./festhest/default.nix { inherit pkgs; };
    in
    { networking.hostName = "festhest";

      networking.firewall.allowedTCPPorts = [ 22 80 ];
      environment.systemPackages = [ festhest ];

      systemd.services.festhest =
        { description = "festhest Webserver";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${festhest}/bin/festhest";
            };
        };
    };
}
```

### Physical Network Configuration

This is the final configuration file, so we're almost out of the woods. This
file describes how to connect with our Amazon AWS account. There are essentially
three relevant variables: the shortname for the keypair used to SSH into EC2
instances, the EC2 region, and the EC2 instance type.

```sh
# ~/festhest/festhest-ec2.nix
let
  region = "eu-central-1";
  accessKeyId = "jezen";

in
{ festhest =
  { resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.instanceType = "t2.micro";
    deployment.ec2.keyPair = resources.ec2KeyPairs.festhest-keys;

  };

  resources.ec2KeyPairs.festhest-keys =
    { inherit region accessKeyId; };
}
```

### Configuring Amazon EC2

The `t2.micro` EC2 instance type might not be available to your account, so it's
worth checking which types you have access to first.

The correct EC2 region can be determined by logging into the EC2 Management
Console with your Amazon AWS account, and checking the subdomain in the address
bar.

![Determining the EC2 region by checking the subdomain in the address
bar](/img/aws-ec2-region.jpg)

While we're still in the EC2 Management Console, we need to configure the
*default* security group to allow inbound connections on port 80. By default, the
*default* security group only allows inbound connections on port 22 (SSH), or
from other EC2 instances associated with the *default* security group.

Navigate to `Network & Security > Security Groups`, select the *default*
security group, open the *Inbound* tab, and click ‘Edit’. A modal window will
appear allowing you to edit inbound rules. Add an HTTP rule with the TCP
protocol and port range 80, then click ‘Save’.

Creating the deployment user and keypair is done through Amazon's IAM (Identity
and Access Management) Console. Navigate over there and create a new IAM user.
Be sure to generate an access key while creating the user, and save the `Access
Key ID` and `Secret Access Key`. I will name my user `jezen`, but of course you
should pick your own name.

Create a new file in your home directory called `~/.ec2-keys`. This file will
contain your deployment user name and access keys. The format of this file is
important; it should adhere to the format `<Access Key ID> <Secret Access Key>
<user>`. As an example:

```sh
AKIAJF7F33KJJJOL3KZQ 3A5A7ty82BBXRHUVsmPBFyvYp5Xc9FsKFMAkYz4u jezen
```

### Deploying To An EC2 Instance

Finally, we have our project and its deployment configured, and we're ready to
push our application to Amazon's servers. We'll use Nix to download and install
the NixOps cloud deployment tool. We'll then use NixOps to create our initial
deployment, and then deploy to Amazon.

```sh
# Install NixOps (about 30 seconds)
nix-env -i nixops
# Create initial deployment
nixops create ./festhest-configuration.nix ./festhest-ec2.nix -d festhest
# Deploy
nixops deploy -d festhest
```

Upload speed makes a profound impact on the total length of time it takes to
deploy. Deploying from my machine took around six hours — which is
excrutiatingly slow — but this is because I had an abysmal upload speed
during testing. There are strategies to mitigate this slowness, like running a
[distributed build][distributedbuild], but I have not tried this.

Testing again with an office connection of ~26Mbps upload, the deploy completed
in 15 minutes.

### Updating The Application

Upon updating the application source code, consequent deploys follow almost
exactly the same procedure as the initial deploy, albeit it takes far less time.

To demonstrate:  Let's change our `~/festhest/festhest/src/Main.hs` file so the
index route responds with `Hello again, Haskell!`, instead of `Hello, Haskell!`.
Write to the file, then drop back into the shell.

Instead of creating a new deploy, we modify our existing one, and then initiate
the deploy process.

```sh
nixops modify ./festhest-configuration.nix ./festhest-ec2.nix -d festhest
nixops deploy -d festhest
```

We can find the public IP address of the EC2 instance from the command line with
NixOps:

```sh
nixops info -d festhest
```

If you point your web browser to the public IP address listed, you should see
your *Hello again, Haskell!* message displayed. And there we have it: a working
Haskell web service!

*FestHest* is Swedish for ‘party horse’.

## Errata

Is this information incorrect, incomplete, or out of date? Please [contact
me][contact] so I can update it. You might just save another developer hours of
hearteache.

## References

The following references proved invaluable when learning how to set all this
stuff up. Big thanks and kudos to the authors for writing all of this down.

- B. Long. [Live-coding/deploying a Yesod webapp][blong]
- R. Blush. [Saluton Haskell & Nix][rblush]
- J.P. Bernardy. [Nix for Haskell Development: HOWTO][jpbernardy]
- J. Matsushita. [Nix + OSX + HaskellNG = Dependency Paradise?][jmatsushita]

[blong]: http://www.boblong.co/live-coding-deploying-a-yesod-webapp/
[rblush]: https://github.com/rowanblush/saluton-hs-nix
[jpbernardy]: http://www.cse.chalmers.se/~bernardy/nix.html
[jmatsushita]: https://iilab.org/news/2015-03-27-nix-osx-haskellng-hakyll.html
[autoenv]: https://github.com/kennethreitz/autoenv
[distributedbuild]: https://nixos.org/wiki/Distributed_build
[contact]: mailto:jezen@jezenthomas.com?subject=Errata
