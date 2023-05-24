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
// import * as helios from '@hyperionbt/helios'


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

  const simpleValidator: SpendingValidator = {
    type: "PlutusV2",
    script:
      "584958470100003233222225335333573466ebc00c008014010401854cd4ccd5cd19baf0033750901300280208030a99a999ab9a3375e0046ea1202c005004100616122002122001120011",
  };

  const vestingContractScript: SpendingValidator = {
    type: "PlutusV1",
    script:
      "<the CBOR HEX of Contract>",
  };

  // const mathBounty :SpendingValidator = {
  //   type:"PlutusV1",
  //   script:
  //     "59084559084201000033233223233223322323233322232333222323333333322222222323332223233332222323233223233322232333222323233223322323233333222223322332233223322332233222232223232533530323330073333573466e1cd55cea802a40004609e6eb4d5d09aab9e5006232635304933573809a09409008e6eb4010cccd5cd19b8735573aa004900011980519191919191919191919191999ab9a3370e6aae754029200023333333333018335026232323333573466e1cd55cea8012400046603c60726ae854008c0acd5d09aba2500223263530593357380ba0b40b00ae26aae7940044dd50009aba1500a33502602735742a012666aa05aeb940b0d5d0a804199aa816bae502c35742a00e66a04c0846ae854018cd4098cd5414c12dd69aba150053232323333573466e1cd55cea80124000466a0406464646666ae68cdc39aab9d5002480008cd40a0cd4105d69aba150023046357426ae8940088c98d4c174cd5ce03082f02e02d89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81319a820bad35742a004608c6ae84d5d1280111931a982e99ab9c06105e05c05b135573ca00226ea8004d5d09aba2500223263530593357380ba0b40b00ae26aae7940044dd50009aba1500433502675c6ae85400ccd4098cd5414dd710009aba150023038357426ae8940088c98d4c154cd5ce02c82b02a02989aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aab9e5001137540026ae854008c8c8c8cccd5cd19b875001480188c074c0e8d5d09aab9e500323333573466e1d400920042301c3044357426aae7940108cccd5cd19b875003480088c070c0bcd5d09aab9e500523333573466e1d401120002301f375c6ae84d55cf280311931a982819ab9c05405104f04e04d04c04b135573aa00226ea8004d5d09aba25002232635304933573809a09409008e2092264c6a609066ae712410350543500049047135573ca00226ea80044dd50009109198008018011000911111111109199999999980080580500480400380300280200180110009109198008018011000891091980080180109000891091980080180109000891091980080180109000909111180200290911118018029091111801002909111180080290008919118011bac001320013550372233335573e0024a01c466a01a60086ae84008c00cd5d100101991919191999ab9a3370e6aae75400d200023330073232323333573466e1cd55cea8012400046601a60626ae854008cd404c0b4d5d09aba25002232635303733573807607006c06a26aae7940044dd50009aba150033335500b75ca0146ae854008cd403dd71aba135744a004464c6a606666ae700dc0d00c80c44d5d1280089aab9e5001137540024442466600200800600440024424660020060044002266aa002eb9d6889119118011bab00132001355031223233335573e0044a012466a01066aa062600c6aae754008c014d55cf280118021aba200302e1357420022244004244244660020080062400224464646666ae68cdc3a800a400046a010600a6ae84d55cf280191999ab9a3370ea00490011280411931a981519ab9c02e02b029028027135573aa00226ea800448488c00800c4488004480048c8c8cccd5cd19b8735573aa004900011980318039aba15002375a6ae84d5d1280111931a981219ab9c028025023022135573ca00226ea80048848cc00400c00880048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98d4c080cd5ce01201080f80f09baa00112232323333573466e1d400520042500723333573466e1d4009200223500a3006357426aae7940108cccd5cd19b87500348000940288c98d4c08ccd5ce01381201101081000f89aab9d50011375400224244460060082244400422444002240024646666ae68cdc3a800a4004400c46666ae68cdc3a80124000400c464c6a603666ae7007c0700680640604d55ce9baa0011220021220012001232323232323333573466e1d4005200c200b23333573466e1d4009200a200d23333573466e1d400d200823300b375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c46601a6eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc048c050d5d0a8049bae357426ae8940248cccd5cd19b875006480088c050c054d5d09aab9e500b23333573466e1d401d2000230133016357426aae7940308c98d4c080cd5ce01201080f80f00e80e00d80d00c80c09aab9d5004135573ca00626aae7940084d55cf280089baa00121222222230070082212222222330060090082122222223005008122222220041222222200322122222223300200900822122222223300100900820012323232323333573466e1d400520022333008375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea00490001180518059aba135573ca00c464c6a602266ae7005404804003c0384d55cea80189aba25001135573ca00226ea80048488c00800c888488ccc00401401000c80048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e5004232635300b33573801e01801401201026aae7540044dd5000909118010019091180080190008891119191999ab9a3370e6aae75400920002335500b300635742a004600a6ae84d5d1280111931a980419ab9c00c009007006135573ca00226ea8005261200120011122123300100300211200149103505431002123001002200111232300100122330033002002001332233322222253353004333573466e1cd4c01c00c8004cdc10010010030028803099ab9c49010857726f6e67205821000051220021220012001212300100220011"
  // };

  const mathBounty: SpendingValidator = {
     type: "PlutusV2",
     script:
       "5907c35907c00100003232332232323232323232323232323322323232323223222323253353232325335333573466e1cd400c8004cdc100100100e80e080e899ab9c4910c57726f6e67206775657373210001c3333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4060064d5d0a80619a80c00c9aba1500b33501801a35742a014666aa038eb9406cd5d0a804999aa80e3ae501b35742a01066a0300466ae85401cccd54070091d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40b9d69aba15002302f357426ae8940088c98c80c4cd5ce01a01881789aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8173ad35742a004605e6ae84d5d1280111931901899ab9c03403102f135573ca00226ea8004d5d09aba2500223263202d33573806005a05626aae7940044dd50009aba1500533501875c6ae854010ccd540700808004d5d0a801999aa80e3ae200135742a00460446ae84d5d1280111931901499ab9c02c029027135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860246ae84d5d1280211931900d99ab9c01e01b019375a00a6666ae68cdc39aab9d5005480008c848c004008dd69aba135573ca00c464c6403266ae7007006405c40604c98c8060cd5ce2490350543500018135573ca00226ea80044dd50008919118011bac001320013550162233335573e0024a014466a01260086ae84008c00cd5d100100a119191999ab9a3370e6aae7540092000233221233001003002300c35742a004600a6ae84d5d1280111931900a19ab9c017014012135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301535742a00466a01e0286ae84d5d1280111931900c99ab9c01c019017135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c806ccd5ce00f00d80c80c00b89aab9d5001137540026ae854008cd402dd71aba135744a004464c6402a66ae7006005404c4d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02644646666aae7c008940208cd401ccc8848cc00400c008c018d55cea80118029aab9e500230043574400602426ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008c8488c00800cc014d5d09aab9e500323333573466e1d40092002212200123263201033573802602001c01a26aae7540044dd5000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201033573802602001c01a01801626aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263200c33573801e01801426aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900519ab9c00d00a00813754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900999ab9c01601301101000f00e00d00c00b135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8030cd5ce00780600500489aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401266ae7003002401c0184d55cea80089baa00112232323333573466e1d400520042122200123333573466e1d40092002232122230030043006357426aae7940108cccd5cd19b87500348000848880088c98c8028cd5ce00680500400380309aab9d5001137540024646666ae68cdc3a800a4004400e46666ae68cdc3a80124000400e464c6400c66ae7002401801000c4d55ce9baa001498480044880084880052410350543100112323001001223300330020020011"
  };

  const typedValidatorScript: SpendingValidator = {
      type: "PlutusV2",
      script:
       "5907dd5907da0100003232332232323232323232323232323322323232323222322323253353232323500225335333573466e1c005205401e01d101e13357389210f57726f6e672072656465656d6572210001d3333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4060064d5d0a80619a80c00c9aba1500b33501801a35742a014666aa038eb9406cd5d0a804999aa80e3ae501b35742a01066a0300466ae85401cccd54070091d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40b9d69aba15002302f357426ae8940088c98c80c4cd5ce01a01881789aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8173ad35742a004605e6ae84d5d1280111931901899ab9c03403102f135573ca00226ea8004d5d09aba2500223263202d33573806005a05626aae7940044dd50009aba1500533501875c6ae854010ccd540700808004d5d0a801999aa80e3ae200135742a00460446ae84d5d1280111931901499ab9c02c029027135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860246ae84d5d1280211931900d99ab9c01e01b0193333573466e1cd55cea802a40004642460020046eb4d5d09aab9e500623263201a33573803a0340306666ae68cdc39aab9d375400c9000100c91931900c99ab9c01c019017101813263201833573892010350543500018135573ca00226ea80044dd50008919118011bac001320013550162233335573e0024a014466a01260086ae84008c00cd5d100100a119191999ab9a3370e6aae7540092000233221233001003002300c35742a004600a6ae84d5d1280111931900a19ab9c017014012135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301535742a00466a01e0286ae84d5d1280111931900c99ab9c01c019017135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c806ccd5ce00f00d80c80c00b89aab9d5001137540026ae854008cd402dd71aba135744a004464c6402a66ae7006005404c4d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02644646666aae7c008940208cd401ccc8848cc00400c008c018d55cea80118029aab9e500230043574400602426ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008c8488c00800cc014d5d09aab9e500323333573466e1d40092002212200123263201033573802602001c01a26aae7540044dd5000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201033573802602001c01a01801626aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263200c33573801e01801426aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900519ab9c00d00a00813754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900999ab9c01601301101000f00e00d00c00b135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8030cd5ce00780600500489aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401266ae7003002401c0184d55cea80089baa00112232323333573466e1d400520042122200123333573466e1d40092002232122230030043006357426aae7940108cccd5cd19b87500348000848880088c98c8028cd5ce00680500400380309aab9d5001137540024646666ae68cdc3a800a4004400e46666ae68cdc3a80124000400e464c6400c66ae7002401801000c4d55ce9baa001498480044880084880052410350543100112323001001223300330020020011"
  };

     const UnitDatum = Data.empty();
     const UnitRedeemer = () => Data.void();
     
  const Datum = (number: number) => Data.to(BigInt(number));
  const Redeemer = (number: number) => Data.to(BigInt(number));

  const lockUtxo = async () => {
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

  const unlockUtxo = async () => {
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


  // const redeemUtxo = async () => {
  //   if (lucid) {
  //     const alwaysSucceedAddress: any = lucid.utils.validatorToAddress(
  //       alwaysSucceedScript,
  //     );
  //     const referenceScriptUtxo = (await lucid.utxosAt(alwaysSucceedAddress)).find(
  //       (utxo) => Boolean(utxo.scriptRef),
  //     );
  //     if (!referenceScriptUtxo) throw new Error("Reference script not found");
    
  //     const utxo = (await lucid.utxosAt(alwaysSucceedAddress)).find((utxo) =>
  //       utxo.datum === Datum() && !utxo.scriptRef
  //     );
    

  //     if (!utxo) throw new Error("Spending script utxo not found");
  //     const tx = await lucid
  //              .newTx()
  //              .readFrom([referenceScriptUtxo]) // spending utxo by reading plutusV2 from reference utxo
  //              .collectFrom([utxo], Redeemer())
  //              .complete();

 
  
  // const mintingPolicy: 

  // const mintNFT = async () => {
  //    if (lucid) {
  //     const policyId: MintingPolicy = lucid.utils.mintingPolicyToId(mintingPolicy);

  //     // const alwaysSucceedAddress: any = lucid.utils.validatorToAddress(
  //     //   alwaysSucceedScript,
  //     // );
  //     const matchingNumberAddress: any = lucid.utils.validatorToAddress(
  //       matchingNumberScript,
  //     );
  //     const tx = await lucid
  //       .newTx()
  //       .mintAssets({[unit: 1n] })
  //       .complete();
      
  //     const signedTx = await tx.sign().complete();
  //     const txHash = await signedTx.submit();
  //   }
  // }
  
  // const vestingDatum : any = new Constr(0,[new Constr (0,[paymentCredential?.hash!]), new Constr (0,[Date.now() + 2628000000])])
  // const vestingRedeemer = Data.empty();

  // const lockVestingUtxo = async () => {    
  //   if (lucid) {
  //     const vestingContractAddress: any = lucid.utils.validatorToAddress(vestingContractScript,);
  //     const recieveing_adr : string = "<some address>"
  //     const {paymentCredential} = lucid?.utils.getAddressDetails(recieveing_adr)
      
  //     const tx = await lucid
  //       .newTx()
  //       .payToContract(vestingContractAddress, Data.to(vestingDatum), { lovelace:BigInt (50000000) })
  //       .complete();
      
  //     const signedTx = await tx.sign().complete();
  //     const txHash = await signedTx.submit();
  //   }
  // }

  const bounty = async () => {    
    if (lucid) {
      const mBountyAddress: any = lucid.utils.validatorToAddress(mathBounty,);
            
      const tx = await lucid
        .newTx()
        .payToContract(mBountyAddress, Datum(100), { lovelace:BigInt (36000000) })
        .payToContract(mBountyAddress, {
            asHash: UnitDatum(),
            scriptRef: mathBounty,
         }, {})
        .complete();
      
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
    }
  }
  
  const solution = async () => {
    if (lucid) {
      const mBountyAddress: any = lucid.utils.validatorToAddress(mathBounty,);
      
      const referenceScriptUtxo = (await lucid.utxosAt(mBountyAddress)).find(
        (utxo) => Boolean(utxo.scriptRef),
      );

      console.log(utxo + " " + typedValidatorAddress);

      if (!referenceScriptUtxo) throw new Error("Reference script not found");

      const utxo = (await lucid.utxosAt(mBountyAddress)).filter((utxo) =>
        utxo.datum === Datum(100) && !utxo.scriptRef
      );

      if (!utxo) throw new Error("Spending script utxo not found");
      const tx = await lucid
               .newTx()
               .readFrom([referenceScriptUtxo])
               .collectFrom(utxo, Redeemer(10))
               .complete();

// const redeemUtxo = async () => {
//   if (lucid) {
//     const alwaysSucceedAddress: any = lucid.utils.validatorToAddress(
//       alwaysSucceedScript,
//     );
//     const referenceScriptUtxo = (await lucid.utxosAt(alwaysSucceedAddress)).find(
//       (utxo) => Boolean(utxo.scriptRef),
//     );
//     if (!referenceScriptUtxo) throw new Error("Reference script not found");
  
//     const utxo = (await lucid.utxosAt(alwaysSucceedAddress)).find((utxo) =>
//       utxo.datum === Datum() && !utxo.scriptRef
//     );
  

//     if (!utxo) throw new Error("Spending script utxo not found");
//     const tx = await lucid
//              .newTx()
//              .readFrom([referenceScriptUtxo]) // spending utxo by reading plutusV2 from reference utxo
//              .collectFrom([utxo], Redeemer())
//              .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      return txHash;
    }
  }
  // const redeemVestingUtxo = async () => {
  //   if (lucid) {
  //     const vestingContractAddress: any = lucid.utils.validatorToAddress(vestingContractScript,);
              
  //     const utxo = (await lucid.utxosAt(vestingContractAddress)).filter((utxo) =>
  //       utxo.datum === Data.to(vestingDatum)
  //     );
 
  //     if (!utxo) throw new Error("Spending script utxo not found");

  //     const tx = await lucid
  //              .newTx()
  //              .collectFrom(utxo, vestingRedeemer)
  //              .attachSpendingValidator(vestingContractScript)
  //              .addSignerKey(paymentCredential?.hash!)
  //              .validFrom(Date.now())
  //              .complete();
        
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
