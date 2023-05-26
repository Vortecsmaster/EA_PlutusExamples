import type { NextPage } from 'next'
import Head from 'next/head'
import WalletConnect from '../components/WalletConnect'
import { useStoreActions, useStoreState } from "../utils/store"
import Link from 'next/link'
import { useState, useEffect } from 'react'
import { getAssets } from "../utils/cardano";
import NftGrid from "../components/NftGrid";
import initLucid from '../utils/lucid'
import { Lucid, TxHash, Lovelace, Constr, SpendingValidator, Data, MintingPolicy } from 'lucid-cardano'



const Helios: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  const [script, setScript] = useState<SpendingValidator>()
  const [scriptAddress, setScriptAddress] = useState("")


  useEffect(() => {
    if (lucid) {
      ;
    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])

  const alwaysSucceedScript: SpendingValidator = {
    type: "PlutusV2",
    script: "49480100002221200101",
  };

  const alwaysFailsScript: SpendingValidator = {
    type: "PlutusV2",
    script: "4746010000222601",
  };

  const redeemer11: SpendingValidator = {
    type: "PlutusV2",
    script: "581e581c0100002225335333573466ebc008dd42402c2440042440022240022d",
  };
  
  const datum22: SpendingValidator = {
    type: "PlutusV2",
    script: "581e581c0100002225335333573466ebc00cdd4240582440042440022240022d",
  };

  const UnitDatum = Data.empty();
  const Datum = (number: number) => Data.to(BigInt(number));
  const Redeemer = (number: number) => Data.to(BigInt(number));

  const give = async () => {
    if (lucid) {
      const simpleValidatorAddress: any = lucid.utils.validatorToAddress(
        simpleValidator,
      
        );
      const tx = await lucid
        .newTx()
        .payToContract(simpleValidatorAddress, Datum(19), { lovelace:BigInt (100000000) })
        .payToContract(simpleValidatorAddress, UnitDatum, { lovelace:BigInt (200000000) })
        .payToContract(simpleValidatorAddress, Datum(19), { lovelace:BigInt (300000000) })
        .payToContract(simpleValidatorAddress, Datum(20), { lovelace:BigInt (400000000) })
        .payToContract(simpleValidatorAddress, Datum(23), { lovelace:BigInt (400000000) })
        .payToContract(simpleValidatorAddress, Datum(23), { lovelace:BigInt (400000000) })
        .complete();
      
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
    }
  }

  const grab = async () => {
    if (lucid) {
      const simpleValidatorAddress: any = lucid.utils.validatorToAddress(
        simpleValidator,
      );
              
      //  const utxos = (await lucid.utxosAt(simpleValidatorAddress)).filter((utxo) =>
      //  utxo.datumHash === lucid.utils.datumToHash(Datum(20))
      //  );
      
      const utxos = await lucid.utxosAt(simpleValidatorAddress);
      
      console.log(utxos);

      if (!utxos) throw new Error("Spending script utxos not found");

      const tx = await lucid
               .newTx()
               .collectFrom(utxos, Redeemer(22))
               .attachSpendingValidator(simpleValidator)
               .complete();
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
             
    return txHash;
                   
  }
}

  const redeemUtxo = async () => {
      if (lucid) {
        const typedValidatorAddress: any = lucid.utils.validatorToAddress(
          typedValidatorScript,
        );
                
        const utxo = (await lucid.utxosAt(typedValidatorAddress)).filter((utxo) =>
          utxo.datum === Datum(19)
        );
        console.log(utxo + " " + typedValidatorAddress);

  
        if (!utxo) throw new Error("Spending script utxo not found");
        const tx = await lucid
                 .newTx()
                 .collectFrom(utxo, Redeemer(19))
                 .attachSpendingValidator(typedValidatorScript)
                 .complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
               
      return txHash;
                     
    }
  }
   
  return (
    <div className="px-10">
      <div className="navbar bg-base-100">
        <div className="flex-1">
          <Link href="/" className="btn btn-ghost normal-case text-xl">Cardano</Link>
        </div>
        <div className="flex-none">
          <WalletConnect />
        </div>
      </div>
      <div>Address: {walletStore.address}</div>
      <div className='m-10'>
        <p> 
          Emurgo example
        </p>
      </div>
      <div className="mx-40 my-10">
        <button className="btn btn-primary m-5" onClick={() => { lockUtxo() }} >Lock</button>
        <button className="btn btn-secondary m-5" onClick={() => { unlockUtxo() }}>Unlock</button>
        {/* <button className="btn btn-secondary m-5" onClick={() => { mintNFT() }}>Mint NFT</button> */}
        <button className="btn btn-secondary m-5" onClick={() => { bounty() }}>Lock math bounty</button>
      </div>
      <div className="mx-40 my-10">
        <label className="label"> Vesting Contract</label> 
        <button className="btn btn-primary m-5" onClick={() => { lockUtxo() }} >Give</button>
        <button className="btn btn-secondary m-5" onClick={() => { redeemUtxo() }}>Grab</button>
        <button className="btn btn-secondary m-5" onClick={() => { solution() }}>GetBounty</button>
      </div>
    </div>
  )
}

export default Helios
