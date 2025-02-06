package com.eazybooks.wishlist.service;

import static com.amazonaws.services.servicediscovery.model.FilterCondition.EQ;

import com.amazonaws.services.servicediscovery.AWSServiceDiscovery;
import com.amazonaws.services.servicediscovery.AWSServiceDiscoveryClientBuilder;
import com.amazonaws.services.servicediscovery.model.InstanceSummary;
import com.amazonaws.services.servicediscovery.model.ListInstancesRequest;
import com.amazonaws.services.servicediscovery.model.ListInstancesResult;
import com.amazonaws.services.servicediscovery.model.ListServicesRequest;
import com.amazonaws.services.servicediscovery.model.ServiceFilter;
import com.amazonaws.services.servicediscovery.model.ServiceFilterName;
import com.amazonaws.services.servicediscovery.model.ServiceSummary;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class AwsServiceUtils {

  private final Logger logger = LoggerFactory.getLogger(AwsServiceUtils.class);

  @Value("${cloudmap.namespaceId}")
  private String namespaceId;

  private final AWSServiceDiscovery client =  AWSServiceDiscoveryClientBuilder.defaultClient();

  public List<InstanceSummary> discoverInstances(String serviceName) {
    try {
      // Get Service ID of MicroserviceB
      String serviceId = getServiceId(serviceName);

      if (serviceId == null) {
        logger.error("Service {} not found in CloudMap", serviceName);
        return Collections.emptyList();
      }
      // List all instances of MicroserviceB
      ListInstancesRequest request = new ListInstancesRequest()
          .withServiceId(serviceId);
      ListInstancesResult result = client.listInstances(request);
      return result.getInstances();
    } catch (Exception e) {
      logger.error("Error discovering instances: {}", e.getMessage());
      return Collections.emptyList();
    }
  }

  private String getServiceId(String serviceName) {
    ListServicesRequest listReq = new ListServicesRequest()
        .withFilters(new ServiceFilter()
            .withName(ServiceFilterName.NAMESPACE_ID)
            .withValues(namespaceId)
            .withCondition(EQ));

    for (ServiceSummary service : client.listServices(listReq).getServices()) {
      if (serviceName.equals(service.getName())) {
        logger.info("Found service with ID {} ", service.getId());
        return service.getId();
      }
    }
    return null;
  }

}
