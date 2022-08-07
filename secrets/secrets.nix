
let
  personal =
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDjXUsGrBVN0jkm39AqfoEIG4PLxmefofNJPUtJeRnIoLZGMaS8Lw/tReVKx64+ttFWLAdkfi+djJHATxwMhhD8BwfJoP5RCz+3P97p1lQh6CjM0XrzTE9Ol6X1/D/mgS4oVa5YaVw3VszxN6Hm2BimKobvfHuIK5w/f0BoBIWxdvs0YyxCJvPsyIfmEvd8CPug9A8bo1/ni77AMpAWuw2RbEBJMk3sxHqUsHlCX/aPTjEqPusictHuy3xoHc4DSxgE/IZkV/d4wOzOUHaM+W8oKvBy8X00rMMprQ1e81WUySkh4UwgplNoD/hHGuVD0EN94ISkjwOfPGW0ACP7bVkZ";
  users = [ personal ];

  leigheas =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILrMG913bhuH9n8yAYd7DtuAHeClFFjQgRPwv0FJUiDy";
  saoiste =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFwI/wXI5lQdgesvIfnenQxUrqbSwCNT3I/ESZ28eQoG";
  nas =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFoyzVr7G3uC7YJI4vH8jhYI+sJcIlcckhwzeMVZOYqn";
  nuc =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP0ssURM8nW08YFCTUk4nCF7wQjvO9kLHJ66w7nwi6B/";
  pi =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH2o+TmAP8oFE4BesbUG6mr+nkak6DmGWHEkkQrjRx4N";
  systems = [ leigheas nas nuc ];
in {
  "wireguard-private-key-leigheas.age".publicKeys = [ personal leigheas ];
  "wireguard-private-key-nas.age".publicKeys = [ personal nas ];
  "wireguard-private-key-nuc.age".publicKeys = [ personal nuc ];
  "smtppw.age".publicKeys = [ personal nas ];
  "deluge.age".publicKeys = [ personal nuc ];
  "nextcloud-db-pass.age".publicKeys = [ personal nuc ];
  "nextcloud-admin-pass.age".publicKeys = [ personal nuc ];
  "ddclient.age".publicKeys = [ personal pi ];
  "fitzmattd-email.age".publicKeys = [ personal leigheas];
  "ftzm-org-email.age".publicKeys = [ personal leigheas ];
}
