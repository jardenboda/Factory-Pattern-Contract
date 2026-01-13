# 🏭 Factory Pattern Contract

A Clarity smart contract demonstrating the factory pattern for cloning and deploying contract templates on the Stacks blockchain.

## 📋 Overview

This contract implements a factory pattern that allows users to:
- 🛠️ Create reusable contract templates
- 🚀 Deploy multiple instances from templates
- 💰 Manage deployment fees and revenue
- 📊 Track deployment statistics
- 🔒 Control access and permissions

## ✨ Features

### 🎯 Core Functionality
- **Template Creation**: Define reusable contract templates with custom fees
- **Contract Deployment**: Deploy new contract instances from existing templates
- **Revenue Management**: Collect fees from deployments and template usage
- **Access Control**: Template creators control their templates' status and fees

### 🔧 Management Features
- **Factory Toggle**: Enable/disable the entire factory system
- **Fee Management**: Adjust deployment fees and template-specific costs
- **Statistics Tracking**: Monitor deployment counts and revenue per template
- **Ownership Transfer**: Transfer factory ownership to new principals

## 🚀 Usage

### Creating a Template

```clarity
(contract-call? .factory-pattern-contract create-template
  "Token Contract"
  "A basic fungible token implementation"
  "(define-fungible-token my-token)"
  u500000)
```

### Deploying from Template

```clarity
(contract-call? .factory-pattern-contract deploy-contract
  u1
  "initial-supply: 1000000")
```

### Managing Templates

```clarity
;; Toggle template status
(contract-call? .factory-pattern-contract toggle-template-status u1)

;; Update template fee
(contract-call? .factory-pattern-contract update-template-fee u1 u750000)
```

## 📊 Contract Functions

### 📝 Write Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-template` | Create a new contract template | name, description, code, fee |
| `deploy-contract` | Deploy instance from template | template-id, init-data |
| `toggle-template-status` | Enable/disable template | template-id |
| `update-template-fee` | Change template deployment fee | template-id, new-fee |
| `deactivate-deployment` | Deactivate deployed contract | contract-id |
| `set-factory-enabled` | Enable/disable factory | enabled |
| `set-deployment-fee` | Set base deployment fee | fee |
| `transfer-ownership` | Transfer factory ownership | new-owner |
| `withdraw-revenue` | Withdraw collected fees | amount |

### 🔍 Read Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-template` | Get template details | Template info |
| `get-deployment` | Get deployment details | Deployment info |
| `get-user-deployments` | Get user's deployments | Contract IDs list |
| `get-template-stats` | Get template statistics | Stats object |
| `get-factory-info` | Get factory configuration | Factory info |
| `get-active-templates` | List active templates | Template IDs |

## 💡 Error Codes

| Code | Description |
|------|-------------|
| `u100` | Unauthorized access |
| `u101` | Invalid amount |
| `u102` | Product not found |
| `u103` | Insufficient funds |
| `u104` | Already exists |
| `u105` | Invalid parameters |
| `u106` | Factory disabled |
| `u107` | Invalid template |
| `u108` | Deployment failed |

## 🛡️ Security Features

- **Access Control**: Only template creators can modify their templates
- **Fee Protection**: Minimum fee requirements prevent spam
- **Factory Toggle**: Owner can disable factory in emergencies
- **Balance Checks**: Ensures sufficient funds before deployment

## 🏗️ Development

### Prerequisites
- Clarinet CLI
- Node.js (for testing)

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy
```

## 📈 Use Cases

- **DeFi Protocols**: Deploy multiple token contracts with different parameters
- **NFT Collections**: Create multiple NFT contracts from a base template
- **DAO Systems**: Deploy governance contracts with varying configurations
- **Gaming**: Create multiple game contract instances with different rules

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

## 🔗 Links

- [Stacks Documentation](https://docs.stacks.co/)
- [Clarity Language Reference](https://docs.stacks.co/clarity)
- [Clarinet Documentation](https://docs.hiro.so/clarinet)
