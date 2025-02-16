package com.eazybooks.authentication;

 import static com.amazonaws.services.servicediscovery.model.FilterCondition.EQ;

 import com.amazonaws.services.servicediscovery.AWSServiceDiscovery;
 import com.amazonaws.services.servicediscovery.AWSServiceDiscoveryClientBuilder;
 import com.amazonaws.services.servicediscovery.model.CreateServiceRequest;
 import com.amazonaws.services.servicediscovery.model.CreateServiceResult;
 import com.amazonaws.services.servicediscovery.model.DeregisterInstanceRequest;
  import com.amazonaws.services.servicediscovery.model.ListServicesRequest;
 import com.amazonaws.services.servicediscovery.model.ListServicesResult;
 import com.amazonaws.services.servicediscovery.model.RegisterInstanceRequest;
 import com.amazonaws.services.servicediscovery.model.ServiceFilter;
 import com.amazonaws.services.servicediscovery.model.ServiceFilterName;
 import com.amazonaws.services.servicediscovery.model.ServiceSummary;
 import com.amazonaws.services.servicediscovery.model.ServiceTypeOption;
 import com.eazybooks.authentication.config.IpAddressResolver;
import jakarta.annotation.PostConstruct;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
 import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
 import org.springframework.stereotype.Component;

@SpringBootApplication
@EnableDiscoveryClient
public class AuthenticationApplication {

  public static void main(String[] args) {
    SpringApplication.run(AuthenticationApplication.class, args);

  }
}