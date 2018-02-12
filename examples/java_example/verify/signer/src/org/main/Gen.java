/*
 *  Signature function
 * 
 */


package org.main;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.*;
import java.io.*;
import java.util.*;
import java.security.spec.PKCS8EncodedKeySpec;

import java.util.Base64;



public class Gen {
 /*   
  *   public static Signature generate() throws Exception {

            // Get instance and initialize a KeyPairGenerator object.
            KeyPairGenerator keyGen = KeyPairGenerator.getInstance("DSA", "SUN");
            SecureRandom random = SecureRandom.getInstance("SHA1PRNG", "SUN");
            keyGen.initialize(512, random);

            // Get a PrivateKey from the generated key pair.
            KeyPair keyPair = keyGen.generateKeyPair();
            // Supply the data to be signed to the Signature object
            // using the update() method and generate the digital
            // signature.
            //
            //
            Files.write(Paths.get("publickey"), keyPair.getPublic().getEncoded());
            Files.write(Paths.get("privatekey"), keyPair.getPrivate().getEncoded());

            byte[] privateKeyBytes = Files.readAllBytes(Paths.get("privatekey"));
            // Get an instance of Signature object and initialize it.
            Signature signature = Signature.getInstance("SHA1withDSA", "SUN");
            
            KeyFactory keyFactory = KeyFactory.getInstance("DSA", "SUN");
            PrivateKey privateKey = keyFactory.generatePrivate(new PKCS8EncodedKeySpec(privateKeyBytes));

            signature.initSign(privateKey);

            return signature;
    }

*/
 
    // API class 
    class Actor {

        Scanner sc = null;

        public Actor(){
            sc = new Scanner(System.in);
        }

        public String read(){
             return sc.nextLine().replaceAll("\\vchunk\\v", "\n");
        }

        public Boolean hasNext(){
             return sc.hasNext();
        }


        public void send(String msg){
             System.out.println(msg.replaceAll("\n", "\\vchunk\\v"));
        }
    
        public void log(String msg){
             System.err.println(System.currentTimeMillis() + ": " + msg);
        }
    
    
    }


 
    public static void main(String[] args) {

        Gen g = new Gen();
        Gen.Actor api = g.new Actor();


        try {

            // init keys context
            Signature signature = getSign();

            // get msg in loop

            while (api.hasNext()) {
                    String msg = api.read();

                     api.log("message get " + msg);

                     // sign byte data
                     byte[] bytes = msg.getBytes();
                     signature.update(bytes);
                     byte[] digitalSignature = signature.sign();

                     // send response
                     api.send(new String(Base64.getEncoder().encode(digitalSignature)));

           }

            
        } catch (Exception e) {
            api.log(e.getMessage());
        }
    }



   public static Signature getSign() throws Exception {

            byte[] privateKeyBytes = Files.readAllBytes(Paths.get("privatekey"));
            // Get an instance of Signature object and initialize it.
            Signature signature = Signature.getInstance("SHA1withDSA", "SUN");
            
            KeyFactory keyFactory = KeyFactory.getInstance("DSA", "SUN");
            PrivateKey privateKey = keyFactory.generatePrivate(new PKCS8EncodedKeySpec(privateKeyBytes));

            signature.initSign(privateKey);

            return signature;

    }



}
