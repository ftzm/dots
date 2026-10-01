{pkgs, ...}: {
  programs.mosh = {
    enable = true;
    # mosh's configure pins -std=gnu++17, but abseil-cpp 20260817 (pulled in by
    # protobuf 36) needs C++20 (std::partial_ordering), so the protoc check fails.
    package = pkgs.mosh.overrideAttrs (_old: {
      env.CXXFLAGS = "-std=gnu++20";
    });
  };
}
