group = "de.hanno.scala"
version = "1.0-SNAPSHOT"

plugins {
    `base`
    id("scala")
    id("antlr")
}

repositories {
    mavenCentral()
}

dependencies {
    antlr("org.antlr:antlr4:4.7.1")
    implementation("org.antlr:antlr4-runtime:4.7.1")
    implementation("org.scala-lang:scala-library:2.11.12")
    implementation("com.github.cretz.kastree:kastree-ast-psi:0.4.0")

    testImplementation("org.scalatest:scalatest_2.11:3.0.0")
    testImplementation("junit:junit:4.12")
}
tasks {
    named<AntlrTask>("generateGrammarSource") {
        maxHeapSize = "64m"
        arguments = arguments + listOf("-visitor", "-long-messages")
    }

    named<ScalaCompile>("compileScala") {
        dependsOn("generateGrammarSource")
    }
    sourceSets {
        getByName("generated") {
            java.srcDir("generated-src/antlr/main/")
        }
    }
//    compileJava.source sourceSets.generated.java, sourceSets.main.java
//
//    clean {
//        delete "generated-src"
//    }

}