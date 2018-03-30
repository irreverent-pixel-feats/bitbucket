# bitbucket [![Build Status](https://img.shields.io/travis/irreverent-pixel-feats/bitbucket.svg?style=flat)](https://travis-ci.org/irreverent-pixel-feats/bitbucket)

## Description

### `TL;DR`

Bitbucket Haskell API

### More

**TODO**: Write a proper description

### Installing the CLI

#### Ubuntu 64-bit

[![Download](https://api.bintray.com/packages/irreverent-pixel-feats/ipf/bitb/images/download.svg) ](https://bintray.com/irreverent-pixel-feats/ipf/bitb/_latestVersion)

``` shell
# Add our APT repo if you havent already
# Install our public key to verify our signed packages
curl -o /tmp/ipf-public-key.asc "https://bintray.com/user/downloadSubjectPublicKey?username=irreverent-pixel-feats"
dos2unix /tmp/ipf-public-key.asc
apt-key add /tmp/ipf-public-key.asc
add-apt-repository "https://dl.bintray.com/irreverent-pixel-feats/ipf xenial main"

# Then actually install the app
apt-get install bitb
```

#### Mac OS X

[![Download](https://api.bintray.com/packages/irreverent-pixel-feats/brew/bitb/images/download.svg) ](https://bintray.com/irreverent-pixel-feats/brew/bitb/_latestVersion)

``` shell
brew tap irreverent-pixel-feats/ipf
brew install bitb
```

## Building the lot

``` shell
bin/ci.common
```

## Building the projects

Each project can be built with the command:

``` shell
./mafia build
```

The first time you ever run it on your system it might take a while, as it will build and install
[`ambiata/mafia`](https://github.com/ambiata/mafia) on your system.

## Running the tests

``` shell
./mafia test
```
