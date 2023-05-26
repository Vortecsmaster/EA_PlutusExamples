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
  
  var Address1:string = "addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt"
  var Address2:string = "addr_test1qqwumapqc9yghg69wvx5rae2s3jz3w5pfs4avw2x96h45pemvtjg092unvw09zgm2az4evmg0anv5xgytk26jxaz2lussnpysf"
  var Address3:string = "addr_test1qp5h55qm05zhv6eapr3em26rurchp9eaxwvt9p69kwuvu42rj9ynfzwggc0s55nlpqegv90w2rshnqf0q3um5pytk4qqutq8j6"
  
  const UnitDatum = Data.empty();
  const Datum = (number: number) => Data.to(BigInt(number));
  const Redeemer = (number: number) => Data.to(BigInt(number));
  
  const simplePaymentTx = async () => {
    if (lucid) {
      const tx = await lucid
        .newTx()
        .payToAddress("addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt", {lovelace: 500000000n})
        .complete();
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
    }
  }

  const simpleValidator: SpendingValidator = {
    type: "PlutusV2",
    script:
      "581c581a0100002225335333573466ebc00c008488008488004448004581",
  };
  
  const notSoSimplePaymentTx = async () => {
    if (lucid) {
      const simpleValidatorAddress: any = lucid.utils.validatorToAddress(simpleValidator,);
      const tx = await lucid
        .newTx()
        .payToAddress(Address1, {lovelace: 110000000n})
        .payToAddress(Address2, {lovelace: 220000000n})
        .payToAddress(Address3, {lovelace: 330000000n})
        .payToContract(simpleValidatorAddress, UnitDatum, { lovelace:BigInt (440000000) })
        .complete();
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
    }
  }
        
  return (
    <div className="px-10">
      <div className="navbar bg-base-100">
        <div className="flex-1">
          <Link href="/" className="btn btn-ghost normal-case text-xl">Emurgo Academy Cardano dApp quickstart</Link>
        </div>
        <div className="flex-none">
          <WalletConnect />
        </div>
      </div>
      <div>Address: {walletStore.address}</div>
      <div className='m-10'>
        <p> 
        Cardano Simple Payment Transactions
        </p>
      </div>
      <div className="mx-40 my-10">
        <button className="btn btn-primary m-5" onClick={() => { simplePaymentTx() }}>Simple Payment</button>
        <button className="btn btn-secondary m-5" onClick={() => { notSoSimplePaymentTx() }}>Multiple Output Payment</button>
       
      </div>
    
    </div>
  )
}

export default Helios
