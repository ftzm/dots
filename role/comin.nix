{inputs, ...}: {
  imports = [inputs.comin.nixosModules.comin];

  services.comin = {
    enable = true;
    remotes = [
      {
        name = "origin";
        url = "https://github.com/ftzm/dots.git";
        branches.main.name = "master";
      }
    ];
  };
}
