ARG VARIANT="jammy"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

# Install necessary packages for SSH agent forwarding
RUN apt-get update && \
    apt-get install -y openssh-client pkg-config libssl-dev


# Create SSH config to use the SSH agent
RUN mkdir -p /home/vscode/.ssh && \
    touch /home/vscode/.ssh/config && \
    echo "Host *\n  ForwardAgent yes" > /home/vscode/.ssh/config && \
    chown -R vscode:vscode /home/vscode/.ssh
