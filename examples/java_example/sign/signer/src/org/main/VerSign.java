package org.main;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.KeyFactory;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;

import java.io.*;
import java.util.*;

import java.util.Base64;
import org.json.JSONObject;


public class VerSign {




    // API class 
    class Actor {

        Scanner sc = null;

        public Actor(){
            sc = new Scanner(System.in);
        }

        public String read(){
             String d = sc.nextLine().replaceAll("\tncm\t", "\n");
             return d;

        }

        public Boolean hasNext(){
             return sc.hasNext();
        }


        public void send(String msg){
             System.out.println(msg.replaceAll("\n", "\tncm\t"));
        }
    
        public void log(String msg){
             System.err.println(System.currentTimeMillis() + ": " + msg);
        }
    
    
    }





 

   public static void main(String[] args) {

        VerSign g = new VerSign();
        VerSign.Actor api = g.new Actor();

        try {

            // init context

            byte[] publicKeyEncoded = Files.readAllBytes(Paths.get("publickey"));

            X509EncodedKeySpec publicKeySpec = new X509EncodedKeySpec(publicKeyEncoded);
            KeyFactory keyFactory = KeyFactory.getInstance("DSA", "SUN");

            PublicKey publicKey = keyFactory.generatePublic(publicKeySpec);
            Signature signature = Signature.getInstance("SHA1withDSA", "SUN");
            signature.initVerify(publicKey);

            // read message
            //
            while (api.hasNext()) {
                    String msg = api.read();

                    api.log("message get " + msg);

                    JSONObject obj = new JSONObject(msg);

                    // do something with every line, one at a time
                    byte[] digitalSignature = Base64.getDecoder().decode(obj.getString("sign").getBytes());
                    byte[] bytes = obj.getString("data").getBytes();

                    signature.update(bytes);

                    boolean verified = signature.verify(digitalSignature);

                    //send result
                    if (verified) {
                        api.send("{\"result\": \"Data verified\"}");
                    } else {
                        api.send("{\"result\": \"Cannot verify data\"}");
                    }
           }

        } catch (Exception e) {
            api.log(e.getMessage());
        }


    }



}
