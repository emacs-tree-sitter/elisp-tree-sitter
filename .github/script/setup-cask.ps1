git clone -b github-actions https://github.com/ubolonton/cask "$env:UserProfile\.cask"
echo "$env:UserProfile\.cask\bin" | Out-File -FilePath $env:GITHUB_PATH -Encoding utf8 -Append
