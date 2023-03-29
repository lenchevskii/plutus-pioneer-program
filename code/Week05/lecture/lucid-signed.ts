import {
    Lucid,
    Blockfrost,
    Address,
    MintingPolicy,
    PolicyId,
    Unit,
    fromText,
    Data,
    getAddressDetails,
    applyParamsToScript
} from "https://deno.land/x/lucid@0.9.1/mod.ts"
import { blockfrostKey, secretSeed } from "./secret.ts"

function readAmount(): bigint {
    const input = prompt("amount: ");
    return input ? BigInt(Number.parseInt(input)) : 1000000n;
}

// set blockfrost endpoint
const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        blockfrostKey
    ),
    "Preprod"
);

// load local stored seed as a wallet into lucid
lucid.selectWalletFromSeed(secretSeed);
const addr: Address = await lucid.wallet.address();
console.log("own address: " + addr);

const pkh: string = getAddressDetails(addr).paymentCredential?.hash || "";
console.log("own pubkey hash: " + pkh);

const Params = Data.Tuple([Data.String]);
type Params = Data.Static<typeof Params>;
const signedPolicy: MintingPolicy = {
    type: "PlutusV2",
    script: applyParamsToScript<Params>(
        "59081a590817010000323232332232323232323232323233223233223232323232322223232533532325335323235002222222222222533533355301612001321233001225335002210031001002501f25335333573466e3c0380040b00ac4d408400454080010840b040a8dd70041a80091001080e899ab9c4901116d697373696e67207369676e61747572650001c3333573466e1cd55cea801a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd405c060d5d0a80619a80b80c1aba1500b33501701935742a014666aa036eb94068d5d0a804999aa80dbae501a35742a01066a02e0486ae85401cccd5406c095d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40bdd69aba150023030357426ae8940088c98c80c8cd5ce01a81a01809aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a817bad35742a00460606ae84d5d1280111931901919ab9c035034030135573ca00226ea8004d5d09aba2500223263202e33573806206005826aae7940044dd50009aba1500533501775c6ae854010ccd5406c0848004d5d0a801999aa80dbae200135742a00460466ae84d5d1280111931901519ab9c02d02c028135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00660266ae84d5d1280191931900e19ab9c01f01e01a3333573466e1cd55ce9baa0044800080748c98c806ccd5ce00f00e80c880e09931900d19ab9c491035054350001c135573ca00226ea8004c8004d5406088448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448c88c008dd6000990009aa80c111999aab9f0012500a233500930043574200460066ae880080608c8c8cccd5cd19b8735573aa004900011991091980080180118071aba150023005357426ae8940088c98c8058cd5ce00c80c00a09aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180b9aba1500233500f016357426ae8940088c98c806ccd5ce00f00e80c89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403a66ae7008007c06c0680644d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201733573803403202a26ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355015223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301613574200222440042442446600200800624464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900919ab9c01501401000f135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01301200e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00f00e00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00680600409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00b00a80880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae7003c0380280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801801600e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7003403002001c0184d55cea80089baa0012323333573466e1d40052002200623333573466e1d40092000200623263200633573801201000800626aae74dd5000a4c2440042440022400292010350543100112323001001223300330020020011",
        [pkh],
        Params)
};

const policyId: PolicyId = lucid.utils.mintingPolicyToId(signedPolicy);
console.log("minting policy: " + policyId);

const unit: Unit = policyId + fromText("PPP Signed");

const amount: bigint = readAmount();

const tx = await lucid
    .newTx()
    .mintAssets({[unit]: amount}, Data.void())
    .attachMintingPolicy(signedPolicy)
    .addSignerKey(pkh)
    .complete();

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
console.log("tid: " + txHash);