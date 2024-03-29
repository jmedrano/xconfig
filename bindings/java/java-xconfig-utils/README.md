# java-xconfig-utils

This library contains utils that simplify dealing with xconfig.
It requires java 1.8
Minimum required xconfig version: >= 1.2.0 (included in java platform >= 3.1.0)

# XConfigNode

This is a class that wraps the XConfig interface simplifying it, and taking advantage of java 1.8 features.

To use it just add the maven dependency:

```xml
<dependency>
    <groupId>com.tuenti.xconfig</groupId>
    <artifactId>java-xconfig-utils</artifactId>
    <version>${java-xconfig-utils.version}</version>
</dependency>
```


Here is an example of usage reading a databaseConfig.yaml:

```java
import com.tuenti.xconfig.XConfigNode;

public class DatabaseConfig {
	private static final int DEFAULT_PORT = 1234;

	private final XConfigNode config;

	@Inject
	public DatabaseConfig(XConfigNode.Factory xconfigFactory) {
		this.config = xconfigFactory.get("databaseConfig");
	}

	public int getPort() {
		return config.getInteger("port")
			.orElse(DEFAULT_PORT);
	}

	public Boolean shouldTestConnectionOnCheckout(String pool) {
		return config.getSubNode("pools", pool)
				.getBoolean("testConnectionOnCheckout")
				.orElse(true);
	}

	public Boolean getLoggingPoolClusterId() {
		return config.getInteger("pools", "logging", "clusterId")
				.orElseThrow(new RuntimeException("Cluster id not found!"));
	}
}
```

And here is an example using breeds:

```java
public class DatabaseConfig {
	(...)
	public int getPort(... enough info to obtain the breeds ...) {
		return config.getBreededNode(breeds)
			.getInteger("port")
			.orElse(DEFAULT_PORT);
	}
}
```

## Release a new version of the library

1. Integrate your branch into master (if it's not done yet)
    ```bash
    $ git checkout master
    $ git pull origin master
    $ git merge YOUR_FEATURE_BRANCH
    ```

1. Create a release branch
    ```bash
    $ git checkout -b release-X.X.X
    ```

1. Set the new release version:
    ```bash
    $ mvn -f src/java-xconfig-utils com.tuenti.maven.plugins:tuentiversions-maven-plugin:set-release
    ```
1. Commit, push, and tag:
    ```bash
    $ git commit -am"Bump pom.xml to new release version"
    $ git tag java-xconfig-utils-X.X.X
    $ git push origin YOUR_RELEASE_BRANCH
    $ git push origin java-xconfig-utils-X.X.X
    ```
1. Deploy to nexus using tu-ci (from the root of the xconfig repository):
    ```bash
    $ tu-ci deploy_java_utils
    ```
1. Once jenkins has finished, set the new development version:
    ```bash
    $ mvn -f src/java-xconfig-utils com.tuenti.maven.plugins:tuentiversions-maven-plugin:set-next-devel
    $ git commit -am"Bump pom.xml to next development version"
    $ git push origin YOUR_RELEASE_BRANCH
    $ git checkout master
    $ git merge YOUR_RELEASE_BRANCH
    $ git push origin master
    ```
