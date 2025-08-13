(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PRODUCT_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_PARAMETERS (err u105))
(define-constant ERR_FACTORY_DISABLED (err u106))
(define-constant ERR_INVALID_TEMPLATE (err u107))
(define-constant ERR_DEPLOYMENT_FAILED (err u108))
(define-constant ERR_VERSION_NOT_FOUND (err u109))
(define-constant ERR_INVALID_VERSION (err u110))

(define-data-var contract-owner principal tx-sender)
(define-data-var factory-enabled bool true)
(define-data-var deployment-fee uint u1000000)
(define-data-var next-product-id uint u1)
(define-data-var total-revenue uint u0)
(define-data-var next-version-id uint u1)

(define-map product-templates
  { template-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    template-code: (string-ascii 10000),
    creation-fee: uint,
    creator: principal,
    active: bool,
    deployments: uint
  }
)

(define-map deployed-contracts
  { contract-id: uint }
  {
    template-id: uint,
    deployer: principal,
    contract-address: principal,
    deployment-block: uint,
    initialization-data: (string-ascii 500),
    active: bool
  }
)

(define-map user-deployments
  { user: principal, template-id: uint }
  { contract-ids: (list 100 uint) }
)

(define-map template-statistics
  { template-id: uint }
  {
    total-deployments: uint,
    total-revenue: uint,
    last-deployment-block: uint
  }
)

(define-map template-versions
  { template-id: uint, version: uint }
  {
    template-code: (string-ascii 10000),
    version-notes: (string-ascii 500),
    created-at: uint,
    active: bool,
    deployments: uint
  }
)

(define-map template-version-info
  { template-id: uint }
  {
    current-version: uint,
    total-versions: uint,
    latest-version: uint
  }
)

(define-public (create-template (name (string-ascii 50)) 
                               (description (string-ascii 200))
                               (template-code (string-ascii 10000))
                               (creation-fee uint))
  (let ((template-id (var-get next-product-id)))
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMETERS)
    (asserts! (> (len template-code) u0) ERR_INVALID_PARAMETERS)
    (asserts! (>= creation-fee u0) ERR_INVALID_PARAMETERS)
    
    (map-set product-templates
      { template-id: template-id }
      {
        name: name,
        description: description,
        template-code: template-code,
        creation-fee: creation-fee,
        creator: tx-sender,
        active: true,
        deployments: u0
      }
    )
    
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: u0,
        total-revenue: u0,
        last-deployment-block: u0
      }
    )
    
    (map-set template-versions
      { template-id: template-id, version: u1 }
      {
        template-code: template-code,
        version-notes: "Initial version",
        created-at: stacks-block-height,
        active: true,
        deployments: u0
      }
    )
    
    (map-set template-version-info
      { template-id: template-id }
      {
        current-version: u1,
        total-versions: u1,
        latest-version: u1
      }
    )
    
    (var-set next-product-id (+ template-id u1))
    (ok template-id)
  )
)

(define-public (deploy-contract (template-id uint)
                               (initialization-data (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (contract-id (var-get next-product-id))
        (total-fee (+ (get creation-fee template) (var-get deployment-fee)))
        (current-stats (default-to
          { total-deployments: u0, total-revenue: u0, last-deployment-block: u0 }
          (map-get? template-statistics { template-id: template-id }))))
    
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (get active template) ERR_INVALID_TEMPLATE)
    (asserts! (>= (stx-get-balance tx-sender) total-fee) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? total-fee tx-sender (var-get contract-owner)))
    
    (map-set deployed-contracts
      { contract-id: contract-id }
      {
        template-id: template-id,
        deployer: tx-sender,
        contract-address: tx-sender,
        deployment-block: stacks-block-height,
        initialization-data: initialization-data,
        active: true
      }
    )
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { deployments: (+ (get deployments template) u1) })
    )
    
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: (+ (get total-deployments current-stats) u1),
        total-revenue: (+ (get total-revenue current-stats) (get creation-fee template)),
        last-deployment-block: stacks-block-height
      }
    )
    
    (let ((current-user-deployments (default-to
            { contract-ids: (list) }
            (map-get? user-deployments { user: tx-sender, template-id: template-id }))))
      (map-set user-deployments
        { user: tx-sender, template-id: template-id }
        { contract-ids: (unwrap! (as-max-len? 
                                  (append (get contract-ids current-user-deployments) contract-id) 
                                  u100) ERR_INVALID_PARAMETERS) }
      )
    )
    
    (var-set total-revenue (+ (var-get total-revenue) total-fee))
    (var-set next-product-id (+ contract-id u1))
    
    (ok contract-id)
  )
)

(define-public (toggle-template-status (template-id uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { active: (not (get active template)) })
    )
    
    (ok (not (get active template)))
  )
)

(define-public (update-template-fee (template-id uint) (new-fee uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (>= new-fee u0) ERR_INVALID_PARAMETERS)
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { creation-fee: new-fee })
    )
    
    (ok new-fee)
  )
)

(define-public (deactivate-deployment (contract-id uint))
  (let ((deployment (unwrap! (map-get? deployed-contracts { contract-id: contract-id }) ERR_PRODUCT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get deployer deployment)) ERR_UNAUTHORIZED)
    
    (map-set deployed-contracts
      { contract-id: contract-id }
      (merge deployment { active: false })
    )
    
    (ok true)
  )
)

(define-public (set-factory-enabled (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set factory-enabled enabled)
    (ok enabled)
  )
)

(define-public (set-deployment-fee (fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (>= fee u0) ERR_INVALID_PARAMETERS)
    (var-set deployment-fee fee)
    (ok fee)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok new-owner)
  )
)

(define-public (withdraw-revenue (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get total-revenue)) ERR_INSUFFICIENT_FUNDS)
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) ERR_INSUFFICIENT_FUNDS)
    
    (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))
    (var-set total-revenue (- (var-get total-revenue) amount))
    (ok amount)
  )
)

(define-public (create-template-version (template-id uint)
                                       (template-code (string-ascii 10000))
                                       (version-notes (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-info (unwrap! (map-get? template-version-info { template-id: template-id }) ERR_VERSION_NOT_FOUND))
        (new-version (+ (get latest-version version-info) u1)))
    
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (> (len template-code) u0) ERR_INVALID_PARAMETERS)
    
    (map-set template-versions
      { template-id: template-id, version: new-version }
      {
        template-code: template-code,
        version-notes: version-notes,
        created-at: stacks-block-height,
        active: true,
        deployments: u0
      }
    )
    
    (map-set template-version-info
      { template-id: template-id }
      {
        current-version: new-version,
        total-versions: (+ (get total-versions version-info) u1),
        latest-version: new-version
      }
    )
    
    (ok new-version)
  )
)

(define-public (deploy-contract-version (template-id uint)
                                       (version uint)
                                       (initialization-data (string-ascii 500)))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-data (unwrap! (map-get? template-versions { template-id: template-id, version: version }) ERR_VERSION_NOT_FOUND))
        (contract-id (var-get next-product-id))
        (total-fee (+ (get creation-fee template) (var-get deployment-fee)))
        (current-stats (default-to
          { total-deployments: u0, total-revenue: u0, last-deployment-block: u0 }
          (map-get? template-statistics { template-id: template-id }))))
    
    (asserts! (var-get factory-enabled) ERR_FACTORY_DISABLED)
    (asserts! (get active template) ERR_INVALID_TEMPLATE)
    (asserts! (get active version-data) ERR_INVALID_VERSION)
    (asserts! (>= (stx-get-balance tx-sender) total-fee) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? total-fee tx-sender (var-get contract-owner)))
    
    (map-set deployed-contracts
      { contract-id: contract-id }
      {
        template-id: template-id,
        deployer: tx-sender,
        contract-address: tx-sender,
        deployment-block: stacks-block-height,
        initialization-data: initialization-data,
        active: true
      }
    )
    
    (map-set template-versions
      { template-id: template-id, version: version }
      (merge version-data { deployments: (+ (get deployments version-data) u1) })
    )
    
    (map-set product-templates
      { template-id: template-id }
      (merge template { deployments: (+ (get deployments template) u1) })
    )
    
    (map-set template-statistics
      { template-id: template-id }
      {
        total-deployments: (+ (get total-deployments current-stats) u1),
        total-revenue: (+ (get total-revenue current-stats) (get creation-fee template)),
        last-deployment-block: stacks-block-height
      }
    )
    
    (let ((current-user-deployments (default-to
            { contract-ids: (list) }
            (map-get? user-deployments { user: tx-sender, template-id: template-id }))))
      (map-set user-deployments
        { user: tx-sender, template-id: template-id }
        { contract-ids: (unwrap! (as-max-len? 
                                  (append (get contract-ids current-user-deployments) contract-id) 
                                  u100) ERR_INVALID_PARAMETERS) }
      )
    )
    
    (var-set total-revenue (+ (var-get total-revenue) total-fee))
    (var-set next-product-id (+ contract-id u1))
    
    (ok contract-id)
  )
)

(define-public (set-current-version (template-id uint) (version uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-data (unwrap! (map-get? template-versions { template-id: template-id, version: version }) ERR_VERSION_NOT_FOUND))
        (version-info (unwrap! (map-get? template-version-info { template-id: template-id }) ERR_VERSION_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    (asserts! (get active version-data) ERR_INVALID_VERSION)
    
    (map-set template-version-info
      { template-id: template-id }
      (merge version-info { current-version: version })
    )
    
    (ok version)
  )
)

(define-public (toggle-version-status (template-id uint) (version uint))
  (let ((template (unwrap! (map-get? product-templates { template-id: template-id }) ERR_PRODUCT_NOT_FOUND))
        (version-data (unwrap! (map-get? template-versions { template-id: template-id, version: version }) ERR_VERSION_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender (get creator template)) ERR_UNAUTHORIZED)
    
    (map-set template-versions
      { template-id: template-id, version: version }
      (merge version-data { active: (not (get active version-data)) })
    )
    
    (ok (not (get active version-data)))
  )
)

(define-read-only (get-template (template-id uint))
  (map-get? product-templates { template-id: template-id })
)

(define-read-only (get-deployment (contract-id uint))
  (map-get? deployed-contracts { contract-id: contract-id })
)

(define-read-only (get-user-deployments (user principal) (template-id uint))
  (map-get? user-deployments { user: user, template-id: template-id })
)

(define-read-only (get-template-stats (template-id uint))
  (map-get? template-statistics { template-id: template-id })
)

(define-read-only (get-factory-info)
  {
    owner: (var-get contract-owner),
    enabled: (var-get factory-enabled),
    deployment-fee: (var-get deployment-fee),
    next-product-id: (var-get next-product-id),
    total-revenue: (var-get total-revenue)
  }
)

(define-read-only (get-factory-stats)
  {
    owner: (var-get contract-owner),
    enabled: (var-get factory-enabled),
    deployment-fee: (var-get deployment-fee),
    next-product-id: (var-get next-product-id),
    total-revenue: (var-get total-revenue)
  }
)

(define-read-only (get-template-version (template-id uint) (version uint))
  (map-get? template-versions { template-id: template-id, version: version })
)

(define-read-only (get-template-version-info (template-id uint))
  (map-get? template-version-info { template-id: template-id })
)

(define-read-only (get-current-template-version (template-id uint))
  (let ((version-info (map-get? template-version-info { template-id: template-id })))
    (match version-info
      info (map-get? template-versions { template-id: template-id, version: (get current-version info) })
      none
    )
  )
)

(define-read-only (get-latest-template-version (template-id uint))
  (let ((version-info (map-get? template-version-info { template-id: template-id })))
    (match version-info
      info (map-get? template-versions { template-id: template-id, version: (get latest-version info) })
      none
    )
  )
)
