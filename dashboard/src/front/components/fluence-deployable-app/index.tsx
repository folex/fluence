import * as React from 'react';
import {connect} from 'react-redux';
import {DeployableApp, DeployableAppId, deployableApps, StorageType} from "../../../fluence/deployable";
import {deploy} from "../../actions/deployable/deploy";
import {Action} from "redux";
import Snippets from "./snippets";
import {cutId, remove0x, toIpfsHash} from "../../../utils";

interface State {
    loading: boolean,
}

interface Props {
    id: DeployableAppId,
    deploy: (app: DeployableApp, appId: string) => Promise<Action>,
    deployedApp: number | undefined,
    deployedAppId: DeployableApp | undefined,
}

class FluenceDeployableApp extends React.Component<Props, State> {
    state: State = {
        loading: false,
    };

    startDeploy = (e: React.MouseEvent<HTMLElement>, app: DeployableApp, appId: string) => {
        this.setState({loading: true});
        this.props.deploy(app, appId)
            .catch(function (err) {
                console.error("error while deploying " + JSON.stringify(err));
            })
            .then(() => this.setState({loading: false}));
    };

    renderAppInfo(app: DeployableApp, appId: string): React.ReactNode {
        let storageHash;
        if (app.storageType == StorageType.Ipfs) {
            storageHash =
                <p className="text-muted" title={app.storageHash}><a
                    href={'http://data.fluence.one:5001/api/v0/cat?arg=' + toIpfsHash(app.storageHash)}
                    title={app.storageHash}
                    target="_blank"
                    rel="noreferrer"
                    download>{cutId(app.storageHash)}</a></p>
                ;
        } else {
            storageHash =
                <p className="text-muted" title={app.storageHash}><a
                    href={'https://swarm-gateways.net/bzz:/' + remove0x(app.storageHash) + '/' + app.name + '.wasm'}
                    title={app.storageHash}
                    target="_blank">{cutId(app.storageHash)}</a></p>
                ;
        }

        return (
            <div className="box-footer no-padding">
                <div className="box-body">
                    <strong><i className="fa fa-bullseye margin-r-5"/>WebAssembly package</strong>

                    {storageHash}

                    <strong><i className="fa fa-bullseye margin-r-5"/>Cluster Size</strong>
                    <p className="text-muted">{app.clusterSize} nodes</p>
                    <hr/>

                    <p>
                        <button
                            type="button"
                            onClick={e => this.startDeploy(e, app, appId)}
                            disabled={!!(this.props.deployedAppId || this.state.loading)}
                            className="btn btn-block btn-success btn-lg">
                            Deploy app <i style={{display: this.state.loading ? 'inline-block' : 'none'}}
                                          className="fa fa-refresh fa-spin"/>
                        </button>
                    </p>
                </div>
            </div>
        );
    }

    render(): React.ReactNode {
        const app = deployableApps[this.props.id];

        return (
            <div>
                <div className="col-md-4 col-xs-12">
                    <div className="box box-widget widget-user-2">
                        <div className="widget-user-header bg-fluence-blue-gradient">
                            <div className="widget-user-image">
                                <span className="entity-info-box-icon entity-info-box-icon-thin"><i
                                    className={app ? 'ion ion-ios-gear-outline' : 'fa fa-refresh fa-spin'}></i></span>
                            </div>
                            <h3 className="widget-user-username">{app.name}</h3>
                        </div>
                        {app && this.renderAppInfo(app, this.props.id)}
                    </div>
                </div>
                <div className="col-md-4 col-xs-12">
                    <Snippets/>
                </div>
            </div>
        );
    }
}

const mapStateToProps = (state: any) => ({
    deployedApp: state.deploy.app,
    deployedAppId: state.deploy.appId,
});

const mapDispatchToProps = {
    deploy
};

export default connect(mapStateToProps, mapDispatchToProps)(FluenceDeployableApp);