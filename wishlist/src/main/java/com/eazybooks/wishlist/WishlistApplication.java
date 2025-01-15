package com.eazybooks.wishlist;

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
import jakarta.annotation.PostConstruct;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.stereotype.Component;

@SpringBootApplication
@EnableDiscoveryClient
public class WishlistApplication {

  public static void main(String[] args) {
    SpringApplication.run(WishlistApplication.class, args);
  }

  @Component
  public class CloudMapRegistration {
    private static final Logger logger = LoggerFactory.getLogger(CloudMapRegistration.class);

    @Value("${cloudmap.namespaceId}")
    private String namespaceId;

    @Value("${spring.application.name}")
    private String serviceName;

    @Value("${server.port}")
    private int servicePort;

    private AWSServiceDiscovery client;

    private String serviceId; // Store the service ID
    private String instanceId; // Store the instance ID

    @PostConstruct
    public void init() throws UnknownHostException {
      client = AWSServiceDiscoveryClientBuilder.defaultClient();
      serviceId = getOrCreateServiceId();
      instanceId = "instance-" + serviceName + "-" + System.currentTimeMillis();
      registerInstance();
      Runtime.getRuntime().addShutdownHook(new Thread(this::deregisterInstance));
    }

    private void registerInstance() throws UnknownHostException {
      // Dynamically fetch the actual private IP address of the machine
      String privateIpAddress = InetAddress.getLocalHost().getHostAddress();
      logger.info("Registering instance with IP: {}" , privateIpAddress);

      try {
        Map<String, String> attributes = Map.of(
            "AWS_INSTANCE_IPV4", privateIpAddress,
            "AWS_INSTANCE_PORT", String.valueOf(servicePort)
        );

        RegisterInstanceRequest registerInstanceRequest = new RegisterInstanceRequest()
            .withServiceId(serviceId)
            .withInstanceId(instanceId)
            .withAttributes(attributes);

        client.registerInstance(registerInstanceRequest);
        logger.info("Registered instance with ID: {} successfully with serviceId: {}", instanceId, serviceId);
      } catch (Exception e) {
        logger.error("Error registering instance: {} ",  e.getMessage());
      }
    }

    private void deregisterInstance() {
      try {
        DeregisterInstanceRequest deregisterInstanceRequest = new DeregisterInstanceRequest()
            .withServiceId(serviceId)
            .withInstanceId(instanceId);
        client.deregisterInstance(deregisterInstanceRequest);
        logger.info("Deregistered instance {} successfully",  instanceId );
      } catch (Exception e) {
        logger.debug("Error deregistering instance: {}", e.getMessage());

      }
    }

    private String getOrCreateServiceId() {
      try {
        ListServicesRequest listServicesRequest = new ListServicesRequest()
            .withFilters(Collections.singletonList(
                new ServiceFilter()
                    .withName(ServiceFilterName.NAMESPACE_ID)
                    .withValues(namespaceId)
                    .withCondition(EQ)));

        ListServicesResult listServicesResult = client.listServices(listServicesRequest);

        for (ServiceSummary serviceSummary : listServicesResult.getServices()) {
          if (serviceSummary.getName().equals(serviceName)) {
            logger.info("Service already exists with ID  {} ", serviceSummary.getId());
            return serviceSummary.getId();
          }
        }

        CreateServiceRequest createServiceRequest = new CreateServiceRequest()
            .withName(serviceName)
            .withNamespaceId(namespaceId)
            .withType(ServiceTypeOption.HTTP);

        CreateServiceResult createServiceResult = client.createService(createServiceRequest);
        logger.info("Created service with ID: {}", createServiceResult.getService().getId());
        return createServiceResult.getService().getId();

      } catch (Exception e) {
        logger.error("Error getting or creating service: {}" ,e.getMessage());

        return null;
      }
    }
  }
}
