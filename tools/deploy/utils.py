# creates register command to register deployed node
def register_command(data, secret_key):
    command = "./fluence register \
        --node_ip            %s \
        --tendermint_key     %s \
        --tendermint_node_id %s \
        --contract_address   %s \
        --account            %s \
        --secret_key         %s \
        --api_port           %s \
        --capacity           %s \
        --eth_url            %s \
        --wait_syncing \
        --gas_price 10 \
        --base64_tendermint_key" % (
            data['node_ip'],
            data['tendermint_key'],
            data['tendermint_node_id'],
            data['contract_address'],
            data['account'],
            secret_key,
            data['api_port'],
            data['capacity'],
            data['ethereum_address']
        )

    return ' '.join(command.split())

def ensure_docker_group(user):
    from fabric.api import run

    run("groupadd docker &>/dev/null || true")
    run("usermod -aG docker %s || true" % user)

def chown_docker_sock(user):
    from fabric.api import run

    run("chmod a+r /var/run/docker.sock")
    run("chown %s:docker /var/run/docker.sock" % user)

def get_docker_pgid():
    from fabric.api import run

    output = run("grep docker /etc/group | cut -d ':' -f 3")
    output = output.stdout.splitlines()
    assert len(output) == 1
    return output[0]
