plugins {
    kotlin("jvm") version "2.3.0"
    application
    alias(libs.plugins.kotlinxSerialization)
}

group = "cvut.fit"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

kotlin {
    jvmToolchain(24)
}

dependencies {
    implementation(libs.kotlinxSerializationJson)
    implementation(libs.kotlinxIoCore)

    testImplementation(kotlin("test"))
}
