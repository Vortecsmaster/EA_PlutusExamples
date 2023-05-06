import { Core } from "../core/mod.js";
import { Transaction, TxHash } from "../types/mod.js";
import { Lucid } from "./lucid.js";
export declare class TxSigned {
    txSigned: Core.Transaction;
    private lucid;
    constructor(lucid: Lucid, tx: Core.Transaction);
    submit(): Promise<TxHash>;
    /** Returns the transaction in Hex encoded Cbor. */
    toString(): Transaction;
    /** Return the transaction hash. */
    toHash(): TxHash;
}
