package com.eazybook.database;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;


//TODO this is health check to personalize users health check
//Low priotity- Implement later
  @RestController
  public class DatabaseHealthCheckController {

    private static final Logger log = LoggerFactory.getLogger(DatabaseHealthCheckController.class);
    @Autowired
    private JdbcTemplate jdbcTemplate;

    @GetMapping("/database-health")
    public ResponseEntity<String> checkDatabaseHealth() {
      try {
        final List<Map<String, Object>> maps = jdbcTemplate.queryForList(
            "SELECT * FROM users LIMIT 1");

maps.forEach(System.out::println);
        if( maps.size()>=0 ){
          return ResponseEntity.status(HttpStatus.OK).body("Database active and accessible ");
        }else{
          return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body("Database not active or not accessible ");
        }
       } catch (Exception e) {
        return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body("Database not available ");
      }
    }
  }

