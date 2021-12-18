FROM mcr.microsoft.com/vscode/devcontainers/dotnet:0-5.0-focal

USER vscode

RUN dotnet tool install -g dotnet-format

RUN echo "export PATH='$PATH:$HOME/.dotnet/tools'" >> ~/.zshrc
