/*
 * Copyright 2019 Fluence Labs Limited
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

use std::result::Result as StdResult;

use derive_getters::Getters;
use ethabi::RawLog;
use ethabi::TopicFilter;
use failure::Error;
use futures::future::Future;
use rand::Rng;
use web3::transports::Http;
use web3::types::FilterBuilder;
use web3::types::H160;
use web3::types::H256;

use fluence::command::EthereumArgs;
use fluence::config::SetupConfig;
use fluence::contract_func::get_transaction_logs;
use fluence::contract_status::get_status;
use fluence::contract_status::status::Status;
use fluence::delete_all::DeleteAll;
use fluence::delete_app::DeleteApp;
use fluence::delete_node::DeleteNode;
use fluence::ethereum_params::EthereumParams;
use fluence::publisher::Published;
use fluence::publisher::Publisher;
use fluence::register::Register;
use fluence::register::Registered;
use fluence::storage::Storage;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;

pub type Result<T> = StdResult<T, Error>;

#[derive(Debug, Getters)]
pub struct TestOpts {
    api_port: u16,
    code_path: PathBuf,
    storage_type: Storage,
    storage_url: String,
    eth: EthereumParams,
}

impl TestOpts {
    pub fn get_code_file() -> PathBuf {
        let mut dir = env::temp_dir();
        dir.push("integration_test.wasm");

        if !dir.exists() {
            let mut f = File::create(dir.clone()).expect("Cannot create a temporary file");
            f.write_all(b"wasm integration test")
                .expect("Cannot write to the temporary file");
            f.flush().expect("Cannot flush temporary file");
        }

        dir
    }

    pub fn default() -> TestOpts {
        let eth = EthereumArgs::default();
        let config = SetupConfig::default().unwrap();
        let params = EthereumParams::generate(eth, config).unwrap();
        let code_path = TestOpts::get_code_file();
        TestOpts {
            api_port: 25000,
            code_path,
            storage_type: Storage::SWARM,
            storage_url: String::from("http://localhost:8500"),
            eth: params,
        }
    }

    pub fn with_eth_sync(mut self, wait: bool) -> Self {
        self.eth.wait_eth_sync = wait;
        self
    }

    pub fn with_tx_include(mut self, wait: bool) -> Self {
        self.eth.wait_tx_include = wait;
        self
    }

    pub fn register_node(
        &mut self,
        ports: u16,
        private: bool,
        tendermint_key: H256,
        tendermint_node_id: H160,
    ) -> Result<(Registered, Register)> {
        let reg = Register::new(
            "127.0.0.1".parse().unwrap(),
            tendermint_key,
            tendermint_node_id,
            self.api_port,
            ports,
            private,
            false,
            self.eth.clone(),
        )
        .unwrap();

        let reg_result = reg.register(false)?;

        Ok((reg_result, reg))
    }

    pub fn register_random_node(
        &mut self,
        ports: u16,
        private: bool,
    ) -> Result<(Option<H256>, Register)> {
        let mut rng = rand::thread_rng();
        let rnd_num: u64 = rng.gen();
        let tendermint_key: H256 = H256::from(rnd_num);
        let tendermint_node_id: H160 = H160::from(rnd_num);

        let (register_result, reg) =
            self.register_node(ports, private, tendermint_key, tendermint_node_id)?;

        let tx = match register_result {
            Registered::TransactionSent(tx) => Some(tx),
            Registered::Deployed {
                app_ids: _,
                ports: _,
                tx,
            } => Some(tx),
            Registered::Enqueued(tx) => Some(tx),
            Registered::AlreadyRegistered => None,
        };

        Ok((tx, reg))
    }

    pub fn publish_app(&self, cluster_size: u8, pin_to: Vec<H256>) -> Result<H256> {
        let publish = Publisher::new(
            self.code_path.clone(),
            self.storage_url.clone(),
            self.storage_type.clone(),
            cluster_size,
            pin_to,
            self.eth.clone(),
        );

        let tx = match publish.publish(false)? {
            Published::TransactionSent(tx) => tx,
            Published::Deployed { app_id: _, tx } => tx,
            Published::Enqueued { app_id: _, tx } => tx,
        };

        Ok(tx)
    }

    // retrieves all events matching `filter`, parsing them through `parse_log`
    // Example usage:
    // use fluence::contract_func::contract::events::app_deployed;
    // get_logs(app_deployed::filter(), app_deployed::parse_log);
    #[cfg(test)]
    pub fn get_logs<T, F>(&self, filter: TopicFilter, parse_log: F) -> Vec<T>
    where
        F: Fn(RawLog) -> ethabi::Result<T>,
    {
        let (_eloop, transport) = Http::new(&self.eth.eth_url.as_str()).unwrap();
        let web3 = web3::Web3::new(transport);
        let filter = FilterBuilder::default()
            .address(vec![self.eth.contract_address])
            .topic_filter(filter)
            .build();
        let filter = web3.eth_filter().create_logs_filter(filter).wait().unwrap();
        let logs = filter.logs().wait().unwrap();
        let logs: Vec<T> = logs
            .into_iter()
            .map(|l| {
                let raw = RawLog::from((l.topics, l.data.0));
                parse_log(raw).unwrap()
            })
            .collect();

        logs
    }

    #[cfg(test)]
    pub fn delete_app(&self, app_id: u64, deployed: bool) -> Result<H256> {
        let delete = DeleteApp::new(app_id, deployed, self.eth.clone());

        delete.delete_app(false)
    }

    #[cfg(test)]
    pub fn delete_node(&self, node_id: H256) -> Result<H256> {
        let delete = DeleteNode::new(node_id, self.eth.clone());

        delete.delete_node(false)
    }

    pub fn get_transaction_logs<T, F>(&self, tx: &H256, parse_log: F) -> Vec<T>
    where
        F: Fn(RawLog) -> ethabi::Result<T>,
    {
        get_transaction_logs(self.eth.eth_url.as_str(), tx, parse_log).unwrap()
    }

    #[cfg(test)]
    pub fn delete_all(&self) {
        let delete = DeleteAll::new(self.eth.clone());
        delete
            .delete_all()
            .expect("failed on calling delete_all (in tests)");
    }

    pub fn get_status(&self) -> Result<Status> {
        get_status(self.eth.eth_url.as_str(), self.eth.contract_address.clone())
    }
}
