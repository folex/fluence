/*
 * Copyright 2018 Fluence Labs Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use failure::Error;
use failure::ResultExt;

use serde_derive::{Deserialize, Serialize};
use web3::types::{Address, H256};

use crate::contract_func::contract::functions::get_app;
use crate::contract_func::contract::functions::get_app_i_ds;
use crate::contract_func::contract::functions::get_node;
use crate::contract_func::contract::functions::get_nodes_ids;
use crate::contract_func::query_contract;
use crate::storage::Storage;
use crate::types::NodeAddress;
use std::convert::Into;
use std::net::IpAddr;
use web3::transports::Http;
use web3::Web3;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct App {
    pub app_id: u64,
    pub storage_hash: H256,
    pub storage_receipt: H256,
    pub storage_type: Storage,
    pub cluster_size: u8,
    pub owner: Address,
    pub pin_to_nodes: Option<Vec<H256>>,
    pub cluster: Option<Cluster>,
}

impl App {
    pub fn new(
        app_id: u64,
        storage_hash: H256,
        storage_receipt: H256,
        storage_type: Storage,
        cluster_size: u8,
        owner: Address,
        pin_to_nodes: Option<Vec<H256>>,
        cluster: Option<Cluster>,
    ) -> App {
        App {
            app_id,
            storage_hash,
            storage_receipt,
            storage_type,
            cluster_size,
            owner,
            pin_to_nodes,
            cluster,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Cluster {
    pub genesis_time: u32,
    pub node_ids: Vec<H256>,
}

impl Cluster {
    pub fn new(genesis_time: u32, node_ids: Vec<H256>) -> Cluster {
        Cluster {
            genesis_time,
            node_ids,
        }
    }
}

/// Represents Fluence node registered in ethereum contract.
/// The node listens to contract events and runs real-time nodes.
/// The purpose of real-time nodes is to host developer's [`App`], e.g., backend code.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Node {
    pub validator_key: H256,
    pub tendermint_p2p_id: String,
    pub ip_addr: IpAddr,
    pub api_port: u16,
    pub capacity: u16,
    pub owner: Address,
    pub is_private: bool,
    pub app_ids: Option<Vec<u64>>, // Defined if loaded
}

impl Node {
    pub fn new(
        id: H256,
        address: NodeAddress,
        api_port: u16,
        capacity: u16,
        owner: Address,
        is_private: bool,
        app_ids: Option<Vec<u64>>,
    ) -> Result<Node, Error> {
        let (tendermint_key, ip_addr) = address.decode()?;
        Ok(Node {
            validator_key: id,
            tendermint_p2p_id: tendermint_key,
            ip_addr,
            api_port,
            capacity,
            owner,
            is_private,
            app_ids,
        })
    }
}

pub fn get_nodes(web3: &Web3<Http>, contract_address: Address) -> Result<Vec<Node>, Error> {
    let (call_data, decoder) = get_nodes_ids::call();
    let node_ids: Vec<H256> = query_contract(call_data, Box::new(decoder), web3, contract_address)?;

    let nodes: Result<Vec<Node>, Error> = node_ids
        .iter()
        .map(|id| {
            let (call_data, decoder) = get_node::call(*id);
            let (ip_addr, api_port, capacity, owner, is_private, app_ids) =
                query_contract(call_data, Box::new(decoder), web3, contract_address)?;

            Node::new(
                *id,
                ip_addr.into(),
                Into::<u64>::into(api_port) as u16,
                Into::<u64>::into(capacity) as u16,
                owner,
                is_private,
                Some(app_ids.into_iter().map(Into::into).collect()),
            )
        })
        .collect();

    Ok(nodes?)
}

pub fn get_apps(web3: &Web3<Http>, contract_address: Address) -> Result<Vec<App>, Error> {
    let (call_data, decoder) = get_app_i_ds::call();
    let app_ids: Vec<u64> = query_contract(call_data, Box::new(decoder), web3, contract_address)
        .context("reading app ids from contract failed")?
        .into_iter()
        .map(Into::into)
        .collect();

    let apps: Result<Vec<App>, Error> = app_ids
        .iter()
        .map(|id| {
            let (call_data, decoder) = get_app::call(*id);
            let (
                storage_hash,
                storage_receipt,
                storage_type_bytes,
                cluster_size,
                owner,
                pin_to,
                genesis,
                node_ids,
            ) = query_contract(call_data, Box::new(decoder), web3, contract_address)
                .context(format!("reading app {} from contract failed", id))?;

            let cluster = if !genesis.is_zero() {
                let genesis: u64 = genesis.into();

                Some(Cluster::new(genesis as u32, node_ids))
            } else {
                None
            };

            let cluster_size: u64 = cluster_size.into();

            let last_byte = storage_type_bytes.0[31];
            let storage_type = Storage::from(last_byte);

            let app = App::new(
                *id,
                storage_hash,
                storage_receipt,
                storage_type,
                cluster_size as u8,
                owner,
                Some(pin_to),
                cluster,
            );

            Ok(app)
        })
        .collect();

    Ok(apps?)
}
