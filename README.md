<p align="center">
  <img src="https://storage.googleapis.com/geniusyield-atlas/logos/atlas-logo-light-mode.svg?sanitize=true#gh-light-mode-only" alt="Atlas Logo" width="425" />
  <h2 align="center">Application backend for Plutus smart contracts on Cardano</h2>
  <p align="center">
    <a href="https://atlas-app.io">
      <img src="https://img.shields.io/badge/-Documentation-blue?style=flat-square&logo=semantic-scholar&logoColor=white" />
    </a>
    <a href="TODO">
      <img src="https://img.shields.io/badge/-Haddock-5E5184?style=flat-square&logo=haskell&logoColor=white" />
    </a>
    <a href="https://cardano.stackexchange.com/questions/tagged/atlas">
      <img src="https://img.shields.io/stackexchange/cardano/t/atlas?style=flat-square" />
    </a>
    <img src="https://img.shields.io/github/commit-activity/m/geniusyield/atlas?style=flat-square" />
    <a href="https://github.com/geniusyield/atlas/blob/main/LICENSE">
      <img src="https://img.shields.io/github/license/geniusyield/atlas?style=flat-square" />
    </a>
    <a href="https://github.com/geniusyield/atlas/actions/workflows/haskell.yml">
      <img src="https://img.shields.io/badge/github%20actions-%232671E5.svg?style=flat-square&logo=githubactions&logoColor=white" />
    </a>
    <a href="./CONTRIBUTING.md">
      <img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square" />
    </a>
    <a href="https://twitter.com/GeniusyieldO">
      <img src="https://img.shields.io/badge/-%40GeniusYieldO-F3F1EF?style=flat-square&logo=twitter&logoColor=1D9BF0" />
    </a>
    <a href="https://discord.gg/TNHf4fs626">
      <img src="https://img.shields.io/badge/-Discord-414EEC?style=flat-square&logo=discord&logoColor=white" />
    </a>
  </p>
</p>

## Table of contents

- 📋 [Documentation](#documentation)
- 🚀 [Features](#features)
- 📝 [Contributing](#contributing)
- ✨ [Credits](#credits)
- ⚖️ [License](#license)

## Documentation

You can find the complete documentation [here](https://atlas-app.io/).

⚡ Learn how to leverage Atlas by building a complete end-to-end dApp: [Getting Started](https://atlas-app.io/getting-started).

We welcome the community to help us improve the documentation by opening pull requests in [Atlas Docs](https://github.com/geniusyield/atlas-docs) repository.

To dive deeper into the Atlas implementation, see its [Haddock](TODO).

## Features

### Easily build transactions
Use an intuitive API to abstract away the complexity around building transactions, balancing UTxOs, and interfacing with Plutus smart contracts.

### Leverage first-class Haskell
Avoid code duplication between on-chain and off-chain code, interoperate with advanced functionalities offered by IOG's Cardano/Plutus libraries, and easily convert between Atlas and Cardano/Plutus types.

### Utilize modular data providers
Query ledger state information from [Maestro](https://www.gomaestro.org/dapp-platform) or host your own [Cardano DB Sync](https://github.com/input-output-hk/cardano-db-sync).

### Test extensively
Use Atlas' test harness to write realistic [unit tests](./getting-started/unit-tests) that correspond to on-chain behavior, and execute [integration tests](./getting-started/integration-tests) in a private network.

### Stay up to date
Benefit from Cardano's latest innovations such as **Reference Inputs**, **Inline Datums** and **Reference Scripts**.

## Contributing

We welcome all contributors! See [contributing guide](./CONTRIBUTING.md) for how to get started.

## Credits

Atlas has been made possible through the support and expertise from:

| Organization Logo | Organization Name |
|-------------------|-------------------|
| <img src="https://storage.googleapis.com/geniusyield-atlas/logos/gy-black.png" width="140" />      | [Genius Yield](https://geniusyield.co)          |
| <img src="https://storage.googleapis.com/geniusyield-atlas/logos/maestro.svg" width="140" />      | [Maestro](https://www.gomaestro.org/)          |
| <img src="https://storage.googleapis.com/geniusyield-atlas/logos/mlabs.svg" width="140" />      | [MLabs](https://mlabs.city/)          |
| <img src="https://storage.googleapis.com/geniusyield-atlas/logos/well-typed.svg" width="140" /> | [Well-Typed](https://well-typed.com/) |
| <img src="https://storage.googleapis.com/geniusyield-atlas/logos/plank.svg" width="140" />      | [Plank](https://www.joinplank.com/)   |

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).
