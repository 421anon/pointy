---
description: "Build steps on compute nodes with their own Nix stores."
---

# Clusters

## Multi-node clusters

When each node keeps its own Nix store, use `pointy-hub` on the head node and `pointy-node` on every compute node. The hub serves its store over SSH to the node keys; each node's `pointy-realise-hub` copies the derivation from the hub, builds it locally, signs the outputs, copies them back to the hub, and uploads the build logs. The hub treats node outputs as untrusted and accepts only paths signed by the listed public keys, so the realise contract still holds across machines.

On the hub, which also runs the backend:

```nix
{ config, pointy, ... }:
{
  imports = [
    pointy.nixosModules.pointy-host
    pointy.nixosModules.pointy-hub
  ];
  services.openssh.enable = true;
  services.pointy-hub = {
    enable = true;
    nodeKeys = [ "ssh-ed25519 AAAA... node@compute-1" ];
    nodePublicKeys = [ "compute-1:AAAA..." ];
  };
  services.pointy-backend.slurm.realiseCommand = config.services.pointy-hub.realiseCommand;
}
```

On every compute node:

```nix
{ pointy, ... }:
{
  imports = [ pointy.nixosModules.pointy-node ];
  services.pointy-node = {
    enable = true;
    signingKeyFile = "/etc/pointy/node-signing-key";
    nodePublicKeys = [ "compute-1:AAAA..." ];
    hub = {
      host = "pointy.example.org";
      sshKeyFile = "/etc/pointy/hub-key";
      hostPublicKey = "AAAA...";
    };
  };
}
```

`nodeKeys` are SSH public keys in `authorized_keys` form; the same keys authenticate to the hub's Nix store as `nix-ssh` and to the log receiver as `logUser`. `nodePublicKeys` are Nix signing public keys, listed on the hub and on every node, including each node's own. Each node's secret `signingKeyFile` and `hub.sshKeyFile` must be readable by the Slurm user that runs the jobs; `hub.hostPublicKey` pins the hub instead of relying on the node's known hosts. The hub needs `services.openssh.enable`; `services.pointy-hub.logUser` defaults to `pointy-log`, and `services.pointy-node.hub.logUser` must match it. The node modules do not configure Slurm themselves, so keep the cluster's `slurm.conf`, job-completion hook, and backend ordering as for any custom cluster.

Build logs are uploaded to `/nix/var/log/nix/drvs/` on the hub, where a backend running there reads them. The hub's node outputs are served back to all nodes as a substituter, with a local build as the fallback.
