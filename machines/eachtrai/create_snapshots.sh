mkdir /pool
mount -t btrfs /dev/mapper/crypted /pool
btrfs subvolume snapshot -r /pool/root /pool/root-blank
btrfs subvolume snapshot -r /pool/home /pool/home-blank

