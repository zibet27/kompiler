package parser.generator

import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import kotlinx.io.writeString
import kotlinx.serialization.json.Json

class ParsingTableCache(
    cacheFile: String,
    cacheLockFile: String,
) {

    val cachePath = Path(cacheFile)
    val cacheLockPath = Path(cacheLockFile)

    /**
     * Save the parsing table to a file.
     */
    fun save(table: ParsingTable, grammarHash: String) {
        val jsonString = json.encodeToString(table)

        SystemFileSystem.sink(cachePath).buffered().use { sink ->
            sink.writeString(jsonString)
        }
        SystemFileSystem.sink(cacheLockPath).buffered().use { sink ->
            sink.writeString(grammarHash)
        }
    }

    /**
     * Load the parsing table from a file.
     * Returns null if the file doesn't exist or can't be loaded.
     */
    fun load(grammarHash: String): ParsingTable? = runCatching {
        if (!SystemFileSystem.exists(cachePath) || !SystemFileSystem.exists(cacheLockPath)) {
            return null
        }
        val prevGrammarHash = SystemFileSystem.source(cacheLockPath).buffered().readString()
        if (prevGrammarHash != grammarHash) {
            return null
        }
        val jsonString = SystemFileSystem.source(cachePath).buffered().readString()
        json.decodeFromString<ParsingTable>(jsonString)
    }.getOrNull()

    companion object {
        private val json = Json {
            prettyPrint = false
            ignoreUnknownKeys = true
            useArrayPolymorphism = true
        }
    }
}