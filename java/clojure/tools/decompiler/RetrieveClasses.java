// Copyright (c) Nicola Mometto & contributors.
// The use and distribution terms for this software are covered by the
// Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
// which can be found in the file epl-v10.html at the root of this distribution.
// By using this software in any fashion, you are agreeing to be bound by
// the terms of this license.
// You must not remove this notice, or any other, from this software.

package clojure.tools.decompiler;

import java.lang.instrument.Instrumentation;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.security.ProtectionDomain;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class RetrieveClasses {

    private static Map<String,byte[]> classes = new ConcurrentHashMap<String,byte[]>();

    public static Map<String,byte[]> getClasses() {
        return classes;
    }

    public static class Transformer implements ClassFileTransformer {
        public byte[] transform(ClassLoader loader, String className,
                                Class<?> classBeingRedefined, ProtectionDomain protectionDomain,
                                byte[] classBytes) throws IllegalClassFormatException {
            classes.put(className, classBytes);
            return classBytes;
        }
    }

    public static void premain(String args, Instrumentation inst) {
        inst.addTransformer(new Transformer());
    }

}
