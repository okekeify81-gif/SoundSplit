
import { describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;
const address3 = accounts.get("wallet_3")!;

const CONTRACT_NAME = "SoundSplitcontract";

describe("SoundSplit - Algorithmic Music Rights Distribution", () => {
  describe("Contract Initialization", () => {
    it("should have correct initial state", () => {
      const platformFee = simnet.callReadOnlyFn(CONTRACT_NAME, "get-platform-fee", [], deployer);
      expect(platformFee.result).toBeUint(250); // 2.5%

      const isPaused = simnet.callReadOnlyFn(CONTRACT_NAME, "is-contract-paused", [], deployer);
      expect(isPaused.result).toBeBool(false);

      const nextTrackId = simnet.callReadOnlyFn(CONTRACT_NAME, "get-next-track-id", [], deployer);
      expect(nextTrackId.result).toBeUint(1);
    });
  });

  describe("Track Registration", () => {
    it("should register a new track successfully", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Test Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmTest123")
      ], deployer);

      expect(result).toBeOk(Cl.uint(1));
    });

    it("should reject track registration with empty title", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8(""),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmTest123")
      ], deployer);

      expect(result).toBeErr(Cl.uint(115)); // ERR_INVALID_INPUT
    });

    it("should reject track registration when contract is paused", () => {
      simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], deployer);

      const { result } = simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Test Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmTest123")
      ], deployer);

      expect(result).toBeErr(Cl.uint(111)); // ERR_CONTRACT_PAUSED

      // Unpause for other tests
      simnet.callPublicFn(CONTRACT_NAME, "unpause-contract", [], deployer);
    });
  });

  describe("Contributor Management", () => {
    beforeEach(() => {
      // Register a track first
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Test Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmTest123")
      ], deployer);
    });

    it("should add contributor successfully", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1), // track-id
        Cl.standardPrincipal(address1), // contributor
        Cl.stringAscii("Vocalist"), // role
        Cl.uint(5000), // 50% contribution
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3") // audio fingerprint
      ], deployer);

      expect(result).toBeOk(Cl.bool(true));
    });

    it("should reject adding contributor to non-existent track", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(999), // non-existent track-id
        Cl.standardPrincipal(address1),
        Cl.stringAscii("Vocalist"),
        Cl.uint(5000),
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);

      expect(result).toBeErr(Cl.uint(101)); // ERR_NOT_FOUND
    });

    it("should reject adding contributor with invalid percentage", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1),
        Cl.standardPrincipal(address1),
        Cl.stringAscii("Vocalist"),
        Cl.uint(15000), // 150% - invalid
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);

      expect(result).toBeErr(Cl.uint(104)); // ERR_INVALID_PERCENTAGE
    });

    it("should reject adding uploader as contributor", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1),
        Cl.standardPrincipal(deployer), // uploader as contributor
        Cl.stringAscii("Vocalist"),
        Cl.uint(5000),
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);

      expect(result).toBeErr(Cl.uint(115)); // ERR_INVALID_INPUT
    });
  });

  describe("Track Locking and Verification", () => {
    beforeEach(() => {
      // Register track and add contributor
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Test Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmTest123")
      ], deployer);

      simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1),
        Cl.standardPrincipal(address1),
        Cl.stringAscii("Vocalist"),
        Cl.uint(10000), // 100%
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);
    });

    it("should lock track successfully", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "lock-track", [
        Cl.uint(1)
      ], deployer);

      expect(result).toBeOk(Cl.bool(true));
    });

    it("should reject locking track without 100% contribution allocation", () => {
      // Add another track with incomplete allocation
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Incomplete Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("c665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmTest456")
      ], deployer);

      simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(2),
        Cl.standardPrincipal(address1),
        Cl.stringAscii("Vocalist"),
        Cl.uint(5000), // Only 50%
        Cl.bufferFromHex("d665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);

      const { result } = simnet.callPublicFn(CONTRACT_NAME, "lock-track", [
        Cl.uint(2)
      ], deployer);

      expect(result).toBeErr(Cl.uint(104)); // ERR_INVALID_PERCENTAGE
    });
  });

  describe("Royalty Distribution", () => {
    beforeEach(() => {
      // Set up complete track with locked status
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Royalty Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmRoyalty")
      ], deployer);

      simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1),
        Cl.standardPrincipal(address1),
        Cl.stringAscii("Vocalist"),
        Cl.uint(7000), // 70%
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);

      simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1),
        Cl.standardPrincipal(address2),
        Cl.stringAscii("Producer"),
        Cl.uint(3000), // 30%
        Cl.bufferFromHex("c665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);

      simnet.callPublicFn(CONTRACT_NAME, "lock-track", [Cl.uint(1)], deployer);
    });

    it("should distribute royalties successfully", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "distribute-royalties", [
        Cl.uint(1),
        Cl.uint(1000000) // 1 STX
      ], deployer);

      expect(result).toBeOk(Cl.bool(true));
    });

    it("should reject royalty distribution on unlocked track", () => {
      // Create another unlocked track
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Unlocked Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("d665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmUnlocked")
      ], deployer);

      const { result } = simnet.callPublicFn(CONTRACT_NAME, "distribute-royalties", [
        Cl.uint(2), // Unlocked track
        Cl.uint(1000000)
      ], deployer);

      expect(result).toBeErr(Cl.uint(106)); // ERR_TRACK_LOCKED
    });
  });

  describe("Dispute Resolution", () => {
    beforeEach(() => {
      // Set up track and contributor
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Dispute Track"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmDispute")
      ], deployer);

      simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1),
        Cl.standardPrincipal(address1),
        Cl.stringAscii("Vocalist"),
        Cl.uint(10000),
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], deployer);

      simnet.callPublicFn(CONTRACT_NAME, "lock-track", [Cl.uint(1)], deployer);

      // Add expert
      simnet.callPublicFn(CONTRACT_NAME, "add-expert", [Cl.standardPrincipal(address2)], deployer);
    });

    it("should create dispute successfully", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "create-dispute", [
        Cl.uint(1),
        Cl.stringUtf8("Incorrect contribution percentage"),
        Cl.stringUtf8("Change vocalist contribution from 100% to 80%")
      ], address1);

      expect(result).toBeOk(Cl.uint(1));
    });

    it("should allow expert voting on dispute", () => {
      // Create dispute
      simnet.callPublicFn(CONTRACT_NAME, "create-dispute", [
        Cl.uint(1),
        Cl.stringUtf8("Incorrect contribution percentage"),
        Cl.stringUtf8("Change vocalist contribution from 100% to 80%")
      ], address1);

      // Vote on dispute
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "vote-on-dispute", [
        Cl.uint(1), // dispute-id
        Cl.bool(true) // vote for
      ], address2);

      expect(result).toBeOk(Cl.bool(true));
    });

    it("should reject dispute creation by non-contributor", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "create-dispute", [
        Cl.uint(1),
        Cl.stringUtf8("Invalid dispute"),
        Cl.stringUtf8("Some change")
      ], address3); // Not a contributor

      expect(result).toBeErr(Cl.uint(102)); // ERR_UNAUTHORIZED
    });
  });

  describe("Security Features", () => {
    it("should enforce rate limiting", () => {
      // First operation should succeed
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Rate Limit Test"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmRateLimit")
      ], deployer);

      // Immediate second operation should fail due to rate limiting
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Rate Limit Test 2"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmRateLimit2")
      ], deployer);

      expect(result).toBeErr(Cl.uint(112)); // ERR_RATE_LIMIT_EXCEEDED
    });

    it("should prevent reentrancy attacks", () => {
      // This would require more complex setup to simulate reentrancy
      // For now, verify that reentrancy guard is properly initialized
      const guardStatus = simnet.callReadOnlyFn(CONTRACT_NAME, "get-reentrancy-guard", [], deployer);
      expect(guardStatus.result).toBeBool(false);
    });

    it("should validate buffer inputs", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Empty Buffer Test"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex(""), // Empty buffer
        Cl.stringUtf8("ipfs://QmTest123")
      ], deployer);

      expect(result).toBeErr(Cl.uint(115)); // ERR_INVALID_INPUT
    });
  });

  describe("Access Control", () => {
    it("should prevent non-owner from pausing contract", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "pause-contract", [], address1);
      expect(result).toBeErr(Cl.uint(100)); // ERR_OWNER_ONLY
    });

    it("should prevent non-owner from adding experts", () => {
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "add-expert", [Cl.standardPrincipal(address3)], address1);
      expect(result).toBeErr(Cl.uint(100)); // ERR_OWNER_ONLY
    });

    it("should prevent non-uploader from managing track", () => {
      // Register track as deployer
      simnet.callPublicFn(CONTRACT_NAME, "register-track", [
        Cl.stringUtf8("Access Control Test"),
        Cl.stringUtf8("Test Artist"),
        Cl.bufferFromHex("a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"),
        Cl.stringUtf8("ipfs://QmAccess")
      ], deployer);

      // Try to add contributor as address1 (not uploader)
      const { result } = simnet.callPublicFn(CONTRACT_NAME, "add-contributor", [
        Cl.uint(1),
        Cl.standardPrincipal(address2),
        Cl.stringAscii("Vocalist"),
        Cl.uint(5000),
        Cl.bufferFromHex("b665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
      ], address1);

      expect(result).toBeErr(Cl.uint(102)); // ERR_UNAUTHORIZED
    });
  });
});
