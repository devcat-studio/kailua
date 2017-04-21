define_msg! { pub PackagePathIsExplicitlySet:
    "ko" => "`kailua.json`에 `package_path`가 이미 설정되어 있어 \
             `package.path`에 대한 대입이 무시됩니다",
    _    => "The assignment to `package.path` will be ignored because \
             `kailua.json` already has an explicit `package_path` value",
}

define_msg! { pub PackageCpathIsExplicitlySet:
    "ko" => "`kailua.json`에 `package_cpath`가 이미 설정되어 있어 \
             `package.cpath`에 대한 대입이 무시됩니다",
    _    => "The assignment to `package.cpath` will be ignored because \
             `kailua.json` already has an explicit `package_cpath` value",
}

