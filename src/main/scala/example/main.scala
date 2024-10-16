import org.web3j.crypto.{ECKeyPair, Keys, Wallet, WalletFile, Credentials}
import org.web3j.crypto.{RawTransaction, TransactionEncoder}
import org.web3j.utils.{Convert, Numeric}
import org.web3j.protocol.core.methods.response.{TransactionReceipt, EthCall}
import org.web3j.protocol.core.methods.request.Transaction
import org.web3j.protocol.Web3j
import org.web3j.protocol.core.methods.response.EthSendTransaction
import org.web3j.protocol.http.HttpService
import org.web3j.protocol.core.methods.response.Web3ClientVersion
import org.web3j.protocol.core.DefaultBlockParameterName
import org.web3j.abi.{FunctionEncoder, FunctionReturnDecoder}
import org.web3j.abi.datatypes.{Type, Function}
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.abi.TypeReference
import org.web3j.tx.{Transfer, RawTransactionManager, TransactionManager}
import org.web3j.tx.gas.DefaultGasProvider

import java.math.{BigInteger, BigDecimal}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import java.util.Collections

/**
  * Manages Ethereum wallets and interactions with Ethereum smart contracts.
  */
class EthereumWalletManager {

  /**
    * Creates a new Ethereum wallet.
    *
    * @return String - The newly created Ethereum address (output).
    */
  def createWallet(): String = {
    val keyPair: ECKeyPair = Keys.createEcKeyPair()
    val walletFile: WalletFile = Wallet.createStandard("YourSecurePassword", keyPair)
    val address: String = "0x" + Keys.getAddress(keyPair)
    println(s"Nouvelle adresse Ethereum : $address")
    val privateKey: String = Numeric.toHexStringNoPrefix(keyPair.getPrivateKey)
    println(s"Clé privée : $privateKey")
    val publicKey: String = Numeric.toHexStringNoPrefix(keyPair.getPublicKey)
    println(s"Clé publique : $publicKey")
    address
  }

  /**
    * Retrieves the balance of a given Ethereum address in Ether.
    *
    * @param address String - Ethereum address to check the balance for (input).
    * @return Unit - Prints the balance in Ether (output).
    */
  def getBalance(address: String): Unit = {
    val balanceFuture = web3.ethGetBalance(address, DefaultBlockParameterName.LATEST).sendAsync()
    balanceFuture.thenAccept(balance => {
      val balanceInEther = Convert.fromWei(balance.getBalance.toString, Convert.Unit.ETHER)
      println(s"Balance de l'adresse $address : $balanceInEther Ether")
    }).exceptionally((e: Throwable) => {
      println(s"Erreur lors de la récupération du solde : ${e.getMessage}")
      null
    })
  }

  /**
    * Checks the connection to the Ethereum network and prints the client version.
    *
    * @return Unit - Prints the Ethereum client version or an error message (output).
    */
  def checkEthConnection(): Unit = {
    val web3ClientVersion = web3.web3ClientVersion().sendAsync()
    web3ClientVersion.thenAccept((version: Web3ClientVersion) => {
      println(s"Connected to Ethereum client version: ${version.getWeb3ClientVersion}")
    }).exceptionally((e: Throwable) => {
      println(s"Failed to connect to Ethereum client: ${e.getMessage}")
      null
    })
  }

  /**
    * Sends Ether from the wallet associated with the given private key to the specified address.
    *
    * @param privateKey String - Private key of the sender (input).
    * @param toAddress String - Recipient Ethereum address (input).
    * @param amountInEther BigDecimal - Amount of Ether to send (input).
    * @return String - Transaction hash of the Ether transfer (output).
    */
  def sendEth(privateKey: String, toAddress: String, amountInEther: BigDecimal): String = {
    val credentials: Credentials = Credentials.create(privateKey)
    val transactionReceipt = Transfer.sendFunds(web3, credentials, toAddress, amountInEther, Convert.Unit.ETHER).send()
    transactionReceipt.getTransactionHash
  }

  /**
    * Deploys a smart contract to the Ethereum blockchain using the specified private key.
    *
    * @param privateKey String - Private key of the deployer (input).
    * @param contractBinary String - Compiled contract bytecode (input).
    * @return String - Address of the deployed contract (output).
    */
  def deployContract(privateKey: String, contractBinary: String): String = {
    val credentials: Credentials = Credentials.create(privateKey)
    val transactionManager = new RawTransactionManager(web3, credentials)
    
    val nonce = web3.ethGetTransactionCount(credentials.getAddress, DefaultBlockParameterName.PENDING).send().getTransactionCount
    val higherGasPrice = gasProvider.getGasPrice.multiply(BigInteger.valueOf(2)) 

    val rawTransaction = RawTransaction.createContractTransaction(
      nonce,
      higherGasPrice,
      gasProvider.getGasLimit,
      BigInteger.ZERO,
      contractBinary
    )

    val signedTransaction = TransactionEncoder.signMessage(rawTransaction, credentials)
    val ethSendTransaction: EthSendTransaction = web3.ethSendRawTransaction(Numeric.toHexString(signedTransaction)).send()

    if (ethSendTransaction.hasError) {
      val errorMessage = ethSendTransaction.getError.getMessage
      println(s"Error sending transaction: $errorMessage")
      
      if (errorMessage.contains("already known")) {
        println("Transaction is already known by the network. Continuing...")
      } else {
        throw new RuntimeException(s"Transaction failed with error: $errorMessage")
      }
    }

    val transactionHash = ethSendTransaction.getTransactionHash

    if (transactionHash == null) {
      throw new RuntimeException("Transaction hash is null. Transaction might have failed.")
    }

    println(s"Transaction hash: $transactionHash")
    val transactionReceipt = waitForTransactionReceipt(transactionHash)
    transactionReceipt.getContractAddress
  }

  /**
    * Waits for the transaction to be confirmed and included in a block, and retrieves its receipt.
    *
    * @param transactionHash String - Hash of the transaction to wait for (input).
    * @param timeoutInSeconds Int - Maximum wait time in seconds (optional, input).
    * @return TransactionReceipt - The transaction receipt once the transaction is confirmed (output).
    * @throws RuntimeException if the transaction is not confirmed within the timeout period.
    */
  def waitForTransactionReceipt(transactionHash: String, timeoutInSeconds: Int = 400): TransactionReceipt = {
    require(transactionHash != null && transactionHash.startsWith("0x"), "Invalid transaction hash")
    println(s"Waiting for transaction receipt for hash: $transactionHash")

    var receipt: TransactionReceipt = null
    val startTime = System.currentTimeMillis()
    val endTime = startTime + timeoutInSeconds * 1000

    while (receipt == null && System.currentTimeMillis() < endTime) {
      Thread.sleep(1000)
      val transactionReceipt = web3.ethGetTransactionReceipt(transactionHash).send()
      if (transactionReceipt.getTransactionReceipt.isPresent) {
        receipt = transactionReceipt.getTransactionReceipt.get()
      } else {
        println(s"Still waiting for transaction $transactionHash to be included in a block...")
      }
    }

    if (receipt == null) {
      throw new RuntimeException(s"Transaction $transactionHash was not included in a block after waiting $timeoutInSeconds seconds")
    }

    receipt
  }

  /**
    * Calls the `set` function on a deployed smart contract to modify a stored value.
    *
    * @param privateKey String - Private key of the caller (input).
    * @param contractAddress String - Address of the deployed smart contract (input).
    * @return TransactionReceipt - The transaction receipt of the function call (output).
    */
  def callSetFunction(privateKey: String, contractAddress: String): TransactionReceipt = {
    val credentials: Credentials = Credentials.create(privateKey)
    val transactionManager = new RawTransactionManager(web3, credentials)

    val function = new Function(
      "set",
      java.util.Arrays.asList(new Uint256(42)),
      Collections.emptyList()
    )

    val encodedFunction = FunctionEncoder.encode(function)

    val transactionHash = transactionManager.sendTransaction(
      gasProvider.getGasPrice,
      gasProvider.getGasLimit,
      contractAddress,
      encodedFunction,
      BigInteger.ZERO
    ).getTransactionHash

    waitForTransactionReceipt(transactionHash)
  }

  /**
    * Calls the `get` function on a deployed smart contract to retrieve a stored value.
    *
    * @param privateKey String - Private key of the caller (input).
    * @param contractAddress String - Address of the deployed smart contract (input).
    * @return BigInteger - The value stored in the contract (output).
    */
  def callGetFunction(privateKey: String, contractAddress: String): BigInteger = {
    val credentials: Credentials = Credentials.create(privateKey)

    val function = new Function(
      "get",
      Collections.emptyList(),
      Collections.singletonList(new TypeReference[Uint256]() {})
    )

    val encodedFunction = FunctionEncoder.encode(function)

    val response: EthCall = web3.ethCall(
      Transaction.createEthCallTransaction(credentials.getAddress, contractAddress, encodedFunction),
      DefaultBlockParameterName.LATEST
    ).send()

    FunctionReturnDecoder.decode(response.getValue, function.getOutputParameters).get(0).asInstanceOf[Uint256].getValue
  }
}
